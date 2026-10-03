;;;; models.lisp — list models of Lisp values, and list views in a few lines
;;;;
;;;; GTK's list widgets display a GListModel of GObjects. GOBJECT:LISP-OBJECT
;;;; is a GObject holding any Lisp value, so a model can hold strings,
;;;; numbers, structures or CLOS instances:
;;;;
;;;;   (gtk:make-list-view (gio:make-list-store :items '("one" "two" "three"))
;;;;     :setup (lambda () (gtk:label-new ""))
;;;;     :bind (lambda (label item) (gtk:label-set-text label item)))

(in-package #:gtk4)

(defclass gobject:lisp-object (gobject:object)
  ((value :initarg :value :initform nil :accessor gobject:lisp-object-value
          :documentation "The Lisp value this object carries."))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispObject")
  (:documentation "A GObject holding any Lisp value, for list models and
anywhere else GTK wants a GObject. Lisp keeps the value alive as long as C
code holds the object."))

(defmethod print-object ((o gobject:lisp-object) stream)
  (print-unreadable-object (o stream :type t)
    (prin1 (gobject:lisp-object-value o) stream)))

(defun gobject:make-lisp-object (value)
  "A new gobject:lisp-object holding VALUE."
  (make-instance 'gobject:lisp-object :value value))

(defun as-gobject (value)
  "VALUE if it is a GObject proxy, else a gobject:lisp-object holding it."
  (if (typep value 'gobject:object) value (gobject:make-lisp-object value)))

(defun lisp-value (object)
  "The Lisp value OBJECT stands for: a lisp-object's value, else OBJECT."
  (if (typep object 'gobject:lisp-object) (gobject:lisp-object-value object) object))

(defun gio:make-list-store (&key items (item-type 'gobject:lisp-object))
  "A new gio:list-store holding ITEMS (a sequence). Items that are not
GObjects are wrapped in gobject:lisp-object. ITEM-TYPE is the class (or
GType) of the store's items."
  (let ((store (gio:list-store-new (if (integerp item-type) item-type (gobject:class-gtype item-type)))))
    (map nil (lambda (item) (gio:list-store-append store (as-gobject item))) items)
    store))

(defun gio:list-model-items (model)
  "The items of MODEL (any gio:list-model) as a list, with
gobject:lisp-object items replaced by their values."
  (loop for i below (gio:list-model-get-n-items model)
        collect (lisp-value (gio:list-model-get-item model i))))

(defun gtk:list-item-value (list-item)
  "The item LIST-ITEM shows, with a gobject:lisp-object replaced by its value.
Accepts a gtk:list-item or a gtk:tree-list-row's item."
  (lisp-value (gtk:list-item-get-item list-item)))

(defun gtk:make-factory (&key setup bind unbind teardown)
  "A gtk:signal-list-item-factory built from Lisp functions:
SETUP () returns a new widget for a row;
BIND (widget value) shows VALUE (the item, unwrapped) in WIDGET;
UNBIND (widget value) and TEARDOWN (widget) undo them, if needed."
  (let ((factory (gtk:signal-list-item-factory-new)))
    (when setup
      (gobject:connect factory :setup
                       (lambda (f item)
                         (declare (ignore f))
                         (gtk:list-item-set-child item (funcall setup)))))
    (when bind
      (gobject:connect factory :bind
                       (lambda (f item)
                         (declare (ignore f))
                         (funcall bind (gtk:list-item-get-child item) (gtk:list-item-value item)))))
    (when unbind
      (gobject:connect factory :unbind
                       (lambda (f item)
                         (declare (ignore f))
                         (funcall unbind (gtk:list-item-get-child item) (gtk:list-item-value item)))))
    (when teardown
      (gobject:connect factory :teardown
                       (lambda (f item)
                         (declare (ignore f))
                         (funcall teardown (gtk:list-item-get-child item)))))
    factory))

(defun gtk:make-list-view (model &key setup bind unbind teardown factory (selection :single))
  "A gtk:list-view showing MODEL (a gio:list-model, or a sequence of Lisp
values). Rows come from FACTORY, or from the functions SETUP, BIND, UNBIND
and TEARDOWN as in gtk:make-factory. SELECTION is :single, :multiple or :none."
  (let* ((model (if (typep model 'sequence) (gio:make-list-store :items model) model))
         (selection-model (ecase selection
                            (:single (gtk:single-selection-new model))
                            (:multiple (gtk:multi-selection-new model))
                            (:none (gtk:no-selection-new model)))))
    (gtk:list-view-new selection-model
                       (or factory (gtk:make-factory :setup setup :bind bind
                                                     :unbind unbind :teardown teardown)))))
