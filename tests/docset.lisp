;;;; docset.lisp — semantic regression tests, run in a fresh generator image.

(defpackage #:gtk4-generator/tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:gen #:gtk4.generator)))

(in-package #:gtk4-generator/tests)

(defvar *generation-context*)
(defvar *docset*)

(define-test docset)

(defun contains (tree text)
  (cond ((stringp tree) (search text tree))
        ((consp tree) (or (contains (car tree) text) (contains (cdr tree) text)))))

(defun entry (id)
  (or (docset:docset-entry *docset* id) (error "Missing docset entry ~a" id)))

(defun keyword-item (entry key)
  (let ((items (cdr (first (docset:entry-section entry "Keyword arguments")))))
    (find key items :test #'equal :key (lambda (item) (second (second (second item)))))))

(define-test all-enum-types-have-value-lists :parent docset
  (let ((count 0))
    (dolist (ns (gen::context-targets *generation-context*))
      (dolist (enum (gen::gir-namespace-enums ns))
        (let* ((qualified (gen::qualify (gen::gir-item-name enum) (gen::gir-namespace-name ns)))
               (spec (list (if (eq (gen::gir-enum-kind enum) :bitfield) :flags :enum)
                           (gen::type-symbol qualified)))
               (inlines (gen::enum-values-inlines *generation-context* spec)))
          (true inlines "Missing values for ~a" qualified)
          (is = (length (gen::gir-enum-members enum)) (count :code inlines :key (lambda (x) (and (consp x) (car x)))))
          (incf count))))
    (true (> count 400) "Check the complete installed GTK stack"))
  (let ((align (entry "method/gtk:widget-set-halign")))
    (dolist (value '(":fill" ":start" ":end" ":center"))
      (true (contains (docset:entry-section align "Syntax") value)))))

(define-test only-inputs-get-accepted-values :parent docset
  (let ((outputs 0))
    (maphash
     (lambda (callable plan)
       (declare (ignore callable))
       (let* ((nsname (gen::namespace-of (gen::symbol-qualified (gen::plan-symbol plan))))
              (ns (find nsname (gen::context-targets *generation-context*) :key #'gen::gir-namespace-name :test #'equal))
              (generated (gen::callable-docset-entry plan ns nil *generation-context*))
              (labels (loop for block in (docset:entry-section generated "Syntax")
                            when (and (eq (car block) :p) (consp (second block))
                                      (eq (car (second block)) :code))
                              collect (second (second block)))))
         (dolist (arg (gen::plan-args plan))
           (when (and (eq (getf (cddr arg) :direction) :out)
                      (member (gen::spec-kind* (second arg)) '(:enum :flags)))
             (incf outputs)
             (false (member (string-downcase (symbol-name (first arg))) labels :test #'equal))))))
     gen::*plans*)
    (true (> outputs 0) "Exercise real output enum parameters")))

(define-test helper-keywords-and-defaults :parent docset
  (let* ((view (entry "function/gtk:make-list-view"))
         (selection (keyword-item view ":selection")))
    (true (contains (docset:entry-section view "Syntax") "&key setup bind unbind teardown factory (selection :single)"))
    (true (contains selection "Default: "))
    (dolist (value '(":single" ":multiple" ":none")) (true (contains selection value)))
    (true (docset:lookup-symbol (list *docset*) "make-list-view" "GTK")))
  (let ((css (entry "function/gtk:add-css")))
    (true (keyword-item css ":display"))
    (true (contains (keyword-item css ":priority") "gtk:+style-provider-priority-application+")))
  (true (keyword-item (entry "function/gobject:connect") ":after"))
  (true (keyword-item (entry "function/gtk4:make-builder") ":package"))
  (true (keyword-item (entry "function/gtk4:save-executable") ":compression"))
  (true (keyword-item (entry "macro/gio:async") ":error"))
  (true (keyword-item (entry "macro/glib:in-main-thread") ":wait"))
  (true (contains (keyword-item (entry "function/adw:show-toast") ":priority") ":high"))
  (true (contains (keyword-item (entry "function/adw:run-application") ":flags") ":handles-open")))

(define-test keyword-destructuring-and-aliases :parent docset
  (let ((specs (gen::lambda-keyword-specs '((var &key ((:timeout seconds) 5)) &body body))))
    (is equal '(:timeout) (mapcar (lambda (spec) (getf spec :key)) specs))
    (is eq 'seconds (getf (first specs) :variable))
    (is = 5 (getf (first specs) :default)))
  (is equal '(:single :multiple :none)
      (gen::ecase-keyword-values 'choice '((let ((x '(a . b)))
                                           (declare (ignore x))
                                           (ecase choice (:single 1) ((:multiple :none) 2)))))))

(define-test inherited-initargs-and-instantiability :parent docset
  (let ((button (docset:entry-section (entry "class/gtk:button") "Initialization"))
        (box (docset:entry-section (entry "class/gtk:box") "Initialization")))
    (true (contains button "MAKE-INSTANCE"))
    (true (contains button ":halign"))
    (true (contains button ":center"))
    (true (contains box ":orientation"))
    (true (contains box ":horizontal"))
    (true (contains box ":vertical")))
  (dolist (id '("interface/gtk:orientable" "class/gtk:widget"))
    (let ((intro (first (docset:entry-section (entry id) "Initialization"))))
      (true (contains intro "cannot be instantiated"))
      (false (contains intro "MAKE-INSTANCE"))))
  (true (contains (docset:entry-section (entry "property/gtk:widget-halign") "Description") ":center")))

(define-test property-overrides-take-precedence :parent docset
  (let ((ctx (gen::%make-context)))
    (setf (gethash "Review.Parent" (gen::context-index ctx))
          (gen::make-gir-class :name "Parent" :kind :class
                              :properties (list (gen::make-gir-property :name "value" :writable t)))
          (gethash "Review.Child" (gen::context-index ctx))
          (gen::make-gir-class :name "Child" :kind :class :parent "Parent"
                              :properties (list (gen::make-gir-property :name "value" :writable nil))))
    (false (gen::class-initarg-properties ctx (gen::lookup ctx "Review.Child") "Review"))))

(define-test docset-format-and-links :parent docset
  (is = 0 (docset:check-docset (docset:docset-directory *docset*)))
  (true (find "Lisp API" (docset:docset-toc *docset*) :key (lambda (x) (getf x :title)) :test #'equal)))

(defun run-tests ()
  (let* ((directory (merge-pathnames (format nil "gtk4-docset-tests-~a/" (symbol-name (gensym)))
                                    (uiop:temporary-directory)))
         (*generation-context* (gen::make-context (gen:load-targets) gen:*target-namespaces*))
         (*docset* nil))
    (unwind-protect
         (progn
           (gen::name-namespaces *generation-context*)
           (gen::build-doc-index *generation-context*)
           (clrhash gen::*plans*)
           (with-open-stream (out (make-broadcast-stream))
             (dolist (ns (gen::context-targets *generation-context*))
               (gen::emit-namespace *generation-context* ns out
                                    :function-stream-maker (lambda (chunk writer)
                                                             (declare (ignore chunk)) (funcall writer out)))))
           (gen::generate-docset *generation-context* directory)
           (setf *docset* (docset:open-docset directory))
           (eq :passed (parachute:status (parachute:test 'docset))))
      (when (probe-file directory)
        (uiop:delete-directory-tree directory :validate t)))))
