;;;; registry.lisp — defining and finding demos

(in-package #:gtk4-demo)

(defstruct demo
  name title category description
  function                              ; () -> a GTK window, not yet presented
  file)                                 ; the source file, shown in the browser

(defvar *demos* (make-hash-table)
  "Demo name (a symbol) -> DEMO.")

(defmacro define-demo (name (&key title category description) &body body)
  "Define a demo. BODY builds and returns a window (without presenting it).
The file containing the definition is shown as the demo's source."
  `(setf (gethash ',name *demos*)
         (make-demo :name ',name
                    :title ,title
                    :category ,category
                    :description ,description
                    :function (lambda () ,@body)
                    :file ,(or *compile-file-truename* *load-truename*))))

(defun all-demos ()
  "Every demo, sorted by category, then title."
  (sort (alexandria:hash-table-values *demos*)
        (lambda (a b)
          (if (string= (demo-category a) (demo-category b))
              (string< (demo-title a) (demo-title b))
              (string< (demo-category a) (demo-category b))))))

(defun find-demo (name) (gethash name *demos*))

(defun make-demo-window (demo)
  "Build DEMO's window."
  (funcall (demo-function demo)))

(defun demo-source (demo)
  (let ((file (demo-file demo)))
    (if (and file (probe-file file))
        (uiop:read-file-string file)
        ";; Source not available.")))

;;; Small helpers shared by the demos

(defun make-demo-frame (title &key (width 480) (height 360))
  "A window titled TITLE, sized WIDTH by HEIGHT."
  (let ((window (gtk:window-new)))
    (gtk:window-set-title window title)
    (gtk:window-set-default-size window width height)
    window))

(defun vbox (spacing &rest children)
  (let ((box (gtk:box-new :vertical spacing)))
    (dolist (c children) (gtk:box-append box c))
    box))

(defun hbox (spacing &rest children)
  (let ((box (gtk:box-new :horizontal spacing)))
    (dolist (c children) (gtk:box-append box c))
    box))

(defun margins (widget pixels)
  "Set all four margins of WIDGET to PIXELS; return WIDGET."
  (gtk:widget-set-margin-top widget pixels)
  (gtk:widget-set-margin-bottom widget pixels)
  (gtk:widget-set-margin-start widget pixels)
  (gtk:widget-set-margin-end widget pixels)
  widget)

(defun scrolled (child)
  (let ((sw (gtk:scrolled-window-new)))
    (gtk:scrolled-window-set-child sw child)
    (gtk:widget-set-vexpand sw t)
    sw))

(defun label-list-factory (text-of)
  "A list item factory showing a label with (TEXT-OF item) for each item."
  (let ((factory (gtk:signal-list-item-factory-new)))
    (gobject:connect factory :setup
                     (lambda (factory item)
                       (declare (ignore factory))
                       (let ((label (gtk:label-new "")))
                         (gtk:label-set-xalign label 0.0)
                         (gtk:widget-set-margin-top label 4)
                         (gtk:widget-set-margin-bottom label 4)
                         (gtk:widget-set-margin-start label 8)
                         (gtk:widget-set-margin-end label 8)
                         (gtk:list-item-set-child item label))))
    (gobject:connect factory :bind
                     (lambda (factory item)
                       (declare (ignore factory))
                       (gtk:label-set-text (gtk:list-item-get-child item)
                                           (funcall text-of (gtk:list-item-get-item item)))))
    factory))
