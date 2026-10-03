;;;; adwaita.lisp — the optional libadwaita bindings

(in-package #:gtk4-tests)

(define-test adwaita :parent gtk4-tests)

(define-test adwaita-loads :parent adwaita
  (true (>= (adw:get-minor-version) 5) "libadwaita 1.~d" (adw:get-minor-version))
  (is string= "adw_toast_new" (gtk4:c-name 'adw:toast-new))
  (true (search "libadwaita/doc/1-latest/class.Toast.html" (gtk4:documentation-url 'adw:toast))))

(define-test style-manager :parent adwaita
  (with-gtk
    (adw:init)
    (let ((old (adw:color-scheme)))
      (unwind-protect
           (progn
             (setf (adw:color-scheme) :force-dark)
             (is eq :force-dark (adw:color-scheme))
             (true (adw:dark-p))
             (setf (adw:color-scheme) :force-light)
             (false (adw:dark-p)))
        (setf (adw:color-scheme) old)))))

(define-test adwaita-widgets-build :parent adwaita
  (with-gtk
    (adw:init)
    (multiple-value-bind (overlay ids)
        (gtk:build
          (adw:toast-overlay
            (adw:toolbar-view
              (adw:header-bar :child-type "top")
              (adw:preferences-page
                (adw:preferences-group :title "Group"
                  (adw:switch-row :id :switch :title "Switch" :active t)
                  (adw:entry-row :id :entry :title "Entry"))))))
      (true (typep (gethash :switch ids) 'adw:switch-row))
      (true (adw:switch-row-get-active (gethash :switch ids)))
      (let ((toast (adw:show-toast overlay "Hello" :timeout 0 :button-label "Undo")))
        (is string= "Hello" (adw:toast-get-title toast))
        (is string= "Undo" (adw:toast-get-button-label toast))
        (adw:toast-dismiss toast)))))

(define-test adwaita-subclass :parent adwaita
  (with-gtk
    (adw:init)
    ;; A Lisp widget deriving from AdwBin. (AdwBin sizes itself with a layout
    ;; manager, so GTK asks that rather than :measure; :contains it calls.)
    ;; GTK only asks mapped widgets, so show it in a window.
    (let ((bin (make-instance 'lisp-bin))
          (window (gtk:window-new)))
      (adw:bin-set-child bin (gtk:label-new "inside"))
      (gtk:window-set-child window bin)
      (gtk:window-present window)
      (iterate-until (lambda () (gtk:widget-get-mapped bin)) :timeout 5)
      (true (gtk:widget-contains bin -5d0 -5d0))
      (is eq :called (lisp-bin-asked bin))
      (gtk:window-destroy window))))

(defclass lisp-bin (adw:bin)
  ((asked :initform nil :accessor lisp-bin-asked))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispAdwBin"))

(gobject:define-vfunc (lisp-bin :contains) (widget x y)
  (declare (ignore x y))
  (setf (lisp-bin-asked widget) :called)
  t)                                    ; contains every point, even outside it
