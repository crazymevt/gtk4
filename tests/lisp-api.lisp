;;;; lisp-api.lisp — the hand-written Lisp layer: list models of Lisp values,
;;;; gtk:build, CSS, gio:async, and GError conditions

(in-package #:gtk4-tests)

(define-test lisp-api :parent gtk4-tests)

(defstruct fruit name color)

(define-test lisp-values-in-models :parent lisp-api
  (let* ((apple (make-fruit :name "apple" :color :red))
         (store (gio:make-list-store :items (list "one" 2 apple))))
    (is = 3 (gio:list-model-get-n-items store))
    (is equal (list "one" 2 apple) (gio:list-model-items store))
    (let ((item (gio:list-model-get-item store 2)))
      (true (typep item 'gobject:lisp-object))
      (is eq apple (gobject:lisp-object-value item)))
    ;; A store of real GObjects keeps them as they are.
    (let* ((actions (list (gio:simple-action-new "a" nil) (gio:simple-action-new "b" nil)))
           (store (gio:make-list-store :items actions :item-type 'gio:simple-action)))
      (is equal actions (gio:list-model-items store)))))

(define-test lisp-values-survive-gc :parent lisp-api
  (let ((store (in-fresh-thread-value
                (lambda () (gio:make-list-store :items (loop for i below 100 collect (list i)))))))
    (collect-until (lambda () nil) :timeout 0.2)
    (is equal (loop for i below 100 collect (list i)) (gio:list-model-items store))))

(defun in-fresh-thread-value (thunk)
  (sb-thread:join-thread (sb-thread:make-thread thunk)))

(define-test error-conditions :parent lisp-api
  (let ((e (handler-case (gio:file-load-contents (gio:file-new-for-path "/nonexistent/file") nil)
             (gio:io-error-not-found (e) e))))
    (true (typep e 'gio:io-error-not-found))
    (true (typep e 'gio:io-error))
    (true (typep e 'glib:glib-error))
    (is eq :not-found (glib:glib-error-keyword e)))
  (true (subtypep 'glib:file-error-noent 'glib:file-error))
  (true (subtypep 'gtk:builder-error-invalid-tag 'gtk:builder-error)))

(define-test async-calls :parent lisp-api
  (let ((file (gio:file-new-for-path (namestring (asdf:system-relative-pathname "gtk4" "LICENSE"))))
        (result nil) (failure nil))
    (gio:async (gio:file-load-contents-async file)
               (lambda (ok contents etag)
                 (declare (ignore ok etag))
                 (setf result (sb-ext:octets-to-string (coerce contents '(vector (unsigned-byte 8)))
                                                       :external-format :utf-8))))
    (gio:async (gio:file-load-contents-async (gio:file-new-for-path "/nonexistent/file") nil)
               (lambda (&rest values) (setf failure values))
               :error (lambda (e) (setf failure e)))
    (iterate-until (lambda () (and result failure)) :timeout 10)
    (true (search "MIT License" result))
    (true (typep failure 'gio:io-error-not-found)))
  ;; Too many arguments before the callback is caught when the form expands.
  (fail (macroexpand-1 '(gio:async (gio:file-load-contents-async f nil :extra) #'print))))

(define-test css-from-sexps :parent lisp-api
  (is string= (format nil ".title {~%  font-size: 20pt;~%  font-weight: bold;~%}~%button {~%  margin: 1px 2px;~%}~%")
      (gtk:css '((".title" :font-size "20pt" :font-weight :bold)
                 ("button" :margin ("1px" "2px")))))
  (is string= (format nil "label { color: red; }~%") (gtk:css '("label { color: red; }"))))

(defvar *build-clicks* 0)

(defun note-build-click (button)
  (declare (ignore button))
  (incf *build-clicks*))

(define-test build-widget-trees :parent lisp-api
  (with-gtk
    (let ((extra (gtk:label-new "made elsewhere")))
      (multiple-value-bind (window ids)
          (gtk:build
            (gtk:window :title "Built" :default-width 320
              (gtk:header-bar :child-type "titlebar"
                (gtk:button :id :start :label "Start" :child-type "start"))
              (gtk:grid :column-spacing 6
                (gtk:label :label "Name" :layout (:column 0 :row 0))
                (gtk:entry :id :name :placeholder-text "Your name" :layout (:column 1 :row 0))
                (gtk:button :id :go :label "Go" :on-clicked 'note-build-click
                            :layout (:column (+ 0 1) :row 1))
                extra)))
        (is string= "Built" (gtk:window-get-title window))
        (is = 320 (gtk:window-default-width window))
        (true (typep (gtk:window-get-titlebar window) 'gtk:header-bar))
        (true (typep (gethash :name ids) 'gtk:entry))
        (let ((grid (gtk:window-get-child window)))
          (is equal '(1 1 1 1) (multiple-value-list (gtk:grid-query-child grid (gethash :go ids))))
          (is eq grid (gtk:widget-get-parent extra)))
        (is eq (gtk:window-get-titlebar window) (gtk:widget-get-ancestor (gethash :start ids)
                                                                      (gobject:class-gtype 'gtk:header-bar)))
        (setf *build-clicks* 0)
        (gobject:emit (gethash :go ids) :clicked)
        (is = 1 *build-clicks*)
        (gtk:window-destroy window))))
  (fail (macroexpand-1 '(gtk:build (not-a-class :x 1)))))

(define-test list-views-of-lisp-values :parent lisp-api
  (with-gtk
    (let* ((bound '())
           (view (gtk:make-list-view (list (make-fruit :name "apple") (make-fruit :name "pear"))
                                     :setup (lambda () (gtk:label-new ""))
                                     :bind (lambda (label fruit)
                                             (push (fruit-name fruit) bound)
                                             (gtk:label-set-text label (fruit-name fruit)))))
           (window (gtk:window-new)))
      (true (typep (gtk:list-view-get-model view) 'gtk:single-selection))
      (gtk:window-set-child window view)
      (gtk:window-present window)
      (iterate-until (lambda () (= 2 (length bound))) :timeout 5)
      (is equal '("apple" "pear") (sort (copy-list bound) #'string<))
      (gtk:window-destroy window))))

(define-test add-css-to-display :parent lisp-api
  (with-gtk
    (let ((provider (gtk:add-css '((".lisp-test" :color :red)))))
      (true (typep provider 'gtk:css-provider))
      (gtk:style-context-remove-provider-for-display (gdk:display-get-default) provider))))
