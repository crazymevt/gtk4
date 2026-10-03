;;;; gtype.lisp — GType queries

(in-package #:gtk4.runtime)

(defun gtype-name (gtype)
  (%g-type-name gtype))

(defun gtype-parent (gtype)
  (let ((p (%g-type-parent gtype)))
    (and (/= p 0) p)))

(defun gtype-fundamental (gtype)
  (%g-type-fundamental gtype))

(defun gtype-is-a (gtype ancestor)
  (%g-type-is-a gtype ancestor))

(defun gtype-from-name (name &optional get-type)
  "The GType named NAME, or NIL if unknown. GET-TYPE, the C name of the type's
_get_type function, registers the type first when GLib has not seen it yet."
  (let ((g (%g-type-from-name name)))
    (when (and (zerop g) get-type)
      (let ((fn (cffi:foreign-symbol-pointer get-type)))
        (unless fn
          (error "gtk4: ~a not found in the loaded libraries" get-type))
        (setf g (cffi:foreign-funcall-pointer fn () gtype))))
    (and (/= g 0) g)))

(defun instance-gtype (pointer)
  "The GType of the GTypeInstance at POINTER (G_TYPE_FROM_INSTANCE)."
  (let ((class (cffi:foreign-slot-value pointer '(:struct gtype-instance) 'g-class)))
    (cffi:foreign-slot-value class '(:struct gtype-class) 'g-type)))

(defun instance-class (pointer)
  "The class structure of the instance at POINTER (G_OBJECT_GET_CLASS)."
  (cffi:foreign-slot-value pointer '(:struct gtype-instance) 'g-class))

(defvar *g-type-gtype* nil)

(defun g-type-gtype ()
  "The GType of GType values themselves (G_TYPE_GTYPE), which is not fundamental."
  (or *g-type-gtype* (setf *g-type-gtype* (%g-gtype-get-type))))
