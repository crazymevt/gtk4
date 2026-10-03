;;;; templates.lisp — composite templates and Lisp signal handlers in .ui files
;;;;
;;;; A Lisp-defined widget class can carry a GtkBuilder template:
;;;;
;;;;   (defclass greeter (gtk:box)
;;;;     ((entry :template-child t :reader greeter-entry))
;;;;     (:metaclass gobject:gobject-class)
;;;;     (:gtype-name "LispGreeter")
;;;;     (:template "<interface><template class=\"LispGreeter\" parent=\"GtkBox\">
;;;;                   <child><object class=\"GtkEntry\" id=\"entry\">
;;;;                     <signal name=\"activate\" handler=\"greet\"/>
;;;;                   </object></child></template></interface>"))
;;;;
;;;; The template is set up when the GType's class is initialized, and
;;;; instantiated for every instance. Signal handlers named in it are Lisp
;;;; functions, found in the package of the class's name.

(in-package #:gtk4)

;;; Resolving handler names to Lisp functions

(defclass lisp-builder-scope (gtk:builder-c-scope gtk:builder-scope)
  ((package :initarg :package :initform *package* :accessor builder-scope-package
            :documentation "Where unqualified handler names are looked up."))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispBuilderScope")
  (:documentation "A GtkBuilderScope that resolves signal handler names in
.ui files and templates to Lisp functions. \"on-save\", \"on_save\" and
\"my-app:on-save\" all name a Lisp symbol; the symbol's function is looked up
at each emission, so redefining it takes effect at once."))

(defun find-handler-symbol (name package)
  "The fbound symbol NAME designates (a symbol name, possibly with
underscores for hyphens, possibly package-qualified), or NIL."
  (let* ((colon (position #\: name))
         (package (if colon
                      (find-package (string-upcase (subseq name 0 colon)))
                      package))
         (name (if colon (string-left-trim ":" (subseq name colon)) name)))
    (when package
      (loop for candidate in (list (string-upcase name)
                                   (string-upcase (substitute #\- #\_ name))
                                   name)
            for symbol = (find-symbol candidate package)
            when (and symbol (fboundp symbol)) return symbol))))

(gobject:define-vfunc (lisp-builder-scope :create-closure)
    (scope builder function-name flags object)
  (declare (ignore builder))
  (let ((symbol (find-handler-symbol function-name (builder-scope-package scope))))
    (if (null symbol)
        ;; Not a Lisp function: let GtkBuilderCScope try its C symbols.
        (call-next-vfunc)
        (let ((swapped (member :swapped flags)))
          (gtk4.runtime:make-closure
           (if object
               ;; With object="...", the handler receives that object first
               ;; and the emitter last (GTK sets :swapped by default then),
               ;; or with swapped="no", the emitter first and the object last.
               (lambda (emitter &rest args)
                 (if swapped
                     (apply symbol object (append args (list emitter)))
                     (apply symbol emitter (append args (list object)))))
               symbol)
           :watch object)))))

(defun make-builder (&key string file resource (package *package*))
  "A new gtk:builder whose signal handlers are Lisp functions in PACKAGE,
loaded from a UI definition STRING, FILE or RESOURCE path (any one)."
  (let ((builder (gtk:builder-new)))
    (gtk:builder-set-scope builder (make-instance 'lisp-builder-scope :package package))
    (cond (string (gtk:builder-add-from-string builder string -1))
          (file (gtk:builder-add-from-file builder (namestring file)))
          (resource (gtk:builder-add-from-resource builder resource)))
    builder))

;;; Templates

(defun template-bytes (class)
  "CLASS's template as a GBytes proxy. The :template class option is the UI
definition itself, or :file PATH [:system NAME] (a relative PATH is relative
to the ASDF system NAME, else to *default-pathname-defaults*), or
:resource PATH."
  (let ((spec (gtk4.runtime::class-template class)))
    (cond
      ((stringp (first spec))
       (glib:bytes-new (sb-ext:string-to-octets (first spec) :external-format :utf-8)))
      ((eq (first spec) :resource)
       (gio:resources-lookup-data (second spec) '()))
      ((eq (first spec) :file)
       (destructuring-bind (file &key system) (rest spec)
         (glib:bytes-new (alexandria:read-file-into-byte-vector
                          (if system
                              (asdf:system-relative-pathname system file)
                              (merge-pathnames file))))))
      (t (error "gtk4: bad :template option for ~s: ~s" (class-name class) spec)))))

(defun template-child-slots (class)
  "The template-child slots CLASS itself declares."
  (remove-if-not (lambda (s)
                   (and (typep s 'gtk4.runtime::gobject-effective-slot-definition)
                        (gtk4.runtime::slot-template-child s)
                        (eq (gtk4.runtime::slot-defining-class s) class)))
                 (sb-mop:class-slots class)))

(defun initialize-template-class (class class-struct)
  (when (gtk4.runtime::class-template class)
    (gtk:widget-class-set-template class-struct (template-bytes class))
    (dolist (slot (template-child-slots class))
      (gtk:widget-class-bind-template-child-full class-struct (gtk4.runtime::slot-template-child slot) nil 0))
    (gtk:widget-class-set-template-scope
     class-struct (make-instance 'lisp-builder-scope
                                 :package (symbol-package (class-name class))))))

(defun initialize-template-instance (class instance)
  (when (and class (gtk4.runtime::class-template class))
    (gtk:widget-init-template instance)))

(pushnew 'initialize-template-class gtk4.runtime::*class-init-hooks*)
(pushnew 'initialize-template-instance gtk4.runtime::*instance-init-hooks*)
