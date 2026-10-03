;;;; gvalue.lisp — converting between GValue and Lisp values
;;;;
;;;; GValues carry property values and signal arguments, so this is the one
;;;; place that knows how every fundamental type maps to Lisp.

(in-package #:gtk4.runtime)

(defmacro with-gvalue ((var gtype) &body body)
  "Bind VAR to a stack GValue initialized to GTYPE, unset when BODY exits."
  `(cffi:with-foreign-object (,var '(:struct gvalue))
     (dotimes (i 3) (setf (cffi:mem-aref ,var :uint64 i) 0))
     (%g-value-init ,var ,gtype)
     (unwind-protect (progn ,@body)
       (%g-value-unset ,var))))

(defun gvalue-type (gvalue)
  (cffi:foreign-slot-value gvalue '(:struct gvalue) 'g-type))

;;; Enum and flags conversion. Generated code registers one converter pair
;;; per enum GType; without one, values stay integers.

(defvar *enum-converters* (make-hash-table)
  "GType -> (TO-LISP . FROM-LISP) for enums and flags.")

(defun register-enum-converter (gtype to-lisp from-lisp)
  (setf (gethash gtype *enum-converters*) (cons to-lisp from-lisp)))

(defun enum-to-lisp (gtype integer)
  (let ((c (gethash gtype *enum-converters*)))
    (if c (funcall (car c) integer) integer)))

(defun enum-from-lisp (gtype value)
  (if (integerp value)
      value
      (let ((c (gethash gtype *enum-converters*)))
        (if c
            (funcall (cdr c) value)
            (error "gtk4: no converter registered for ~a values; pass an integer"
                   (gtype-name gtype))))))

;;; Reading

(defun null-to-nil (pointer)
  (unless (cffi:null-pointer-p pointer) pointer))

(defun foreign-string-or-nil (pointer)
  (if (cffi:null-pointer-p pointer) nil (cffi:foreign-string-to-lisp pointer)))

(defun gvalue-get (gvalue)
  "The Lisp value held in GVALUE. Objects are wrapped (sharing ownership),
boxed values are copied into a BOXED proxy, strings are copied."
  (let ((type (gvalue-type gvalue)))
    (cond
      ((= type (g-type-gtype)) (%g-value-get-gtype gvalue))
      ((gtype-is-a type +g-type-object+)
       (wrap-object (%g-value-get-object gvalue) :transfer :none))
      (t
       (let ((fundamental (gtype-fundamental type)))
         (cond
           ((= fundamental +g-type-boolean+) (%g-value-get-boolean gvalue))
           ((= fundamental +g-type-int+) (%g-value-get-int gvalue))
           ((= fundamental +g-type-uint+) (%g-value-get-uint gvalue))
           ((= fundamental +g-type-double+) (%g-value-get-double gvalue))
           ((= fundamental +g-type-float+) (%g-value-get-float gvalue))
           ((= fundamental +g-type-string+) (foreign-string-or-nil (%g-value-get-string gvalue)))
           ((= fundamental +g-type-enum+) (enum-to-lisp type (%g-value-get-enum gvalue)))
           ((= fundamental +g-type-flags+) (enum-to-lisp type (%g-value-get-flags gvalue)))
           ((= fundamental +g-type-long+) (%g-value-get-long gvalue))
           ((= fundamental +g-type-ulong+) (%g-value-get-ulong gvalue))
           ((= fundamental +g-type-int64+) (%g-value-get-int64 gvalue))
           ((= fundamental +g-type-uint64+) (%g-value-get-uint64 gvalue))
           ((= fundamental +g-type-char+) (%g-value-get-schar gvalue))
           ((= fundamental +g-type-uchar+) (%g-value-get-uchar gvalue))
           ((= fundamental +g-type-boxed+)
            (wrap-boxed (%g-value-get-boxed gvalue) type :transfer :none))
           ((= fundamental +g-type-pointer+) (null-to-nil (%g-value-get-pointer gvalue)))
           ((= fundamental +g-type-variant+) (null-to-nil (%g-value-get-variant gvalue)))
           ((= fundamental +g-type-param+) (null-to-nil (%g-value-get-param gvalue)))
           ((= fundamental +g-type-none+) nil)
           (t (error "gtk4: cannot convert a GValue of type ~a" (gtype-name type)))))))))

;;; Writing

(defun set-gvalue (gvalue value)
  "Store the Lisp VALUE into GVALUE, which must already be initialized to
its type. Objects and boxed values may be proxies or raw foreign pointers."
  (let ((type (gvalue-type gvalue)))
    (cond
      ((= type (g-type-gtype)) (%g-value-set-gtype gvalue value))
      ((gtype-is-a type +g-type-object+) (%g-value-set-object gvalue (object-pointer value)))
      (t
       (let ((fundamental (gtype-fundamental type)))
         (cond
           ((= fundamental +g-type-boolean+) (%g-value-set-boolean gvalue (and value t)))
           ((= fundamental +g-type-int+) (%g-value-set-int gvalue value))
           ((= fundamental +g-type-uint+) (%g-value-set-uint gvalue value))
           ((= fundamental +g-type-double+) (%g-value-set-double gvalue (float value 1d0)))
           ((= fundamental +g-type-float+) (%g-value-set-float gvalue (float value 1f0)))
           ((= fundamental +g-type-string+)
            (if value
                (%g-value-set-string gvalue value)
                (%g-value-set-pointer-string gvalue)))
           ((= fundamental +g-type-enum+) (%g-value-set-enum gvalue (enum-from-lisp type value)))
           ((= fundamental +g-type-flags+) (%g-value-set-flags gvalue (enum-from-lisp type value)))
           ((= fundamental +g-type-long+) (%g-value-set-long gvalue value))
           ((= fundamental +g-type-ulong+) (%g-value-set-ulong gvalue value))
           ((= fundamental +g-type-int64+) (%g-value-set-int64 gvalue value))
           ((= fundamental +g-type-uint64+) (%g-value-set-uint64 gvalue value))
           ((= fundamental +g-type-char+) (%g-value-set-schar gvalue value))
           ((= fundamental +g-type-uchar+) (%g-value-set-uchar gvalue value))
           ((= fundamental +g-type-boxed+) (%g-value-set-boxed gvalue (object-pointer value)))
           ((= fundamental +g-type-pointer+) (%g-value-set-pointer gvalue (object-pointer value)))
           ((= fundamental +g-type-variant+) (%g-value-set-variant gvalue (object-pointer value)))
           ((= fundamental +g-type-param+) (%g-value-set-param gvalue (object-pointer value)))
           (t (error "gtk4: cannot store into a GValue of type ~a" (gtype-name type))))))))
  value)

(defun %g-value-set-pointer-string (gvalue)
  "Set a string GValue to NULL."
  (cffi:foreign-funcall "g_value_set_string" :pointer gvalue :pointer (cffi:null-pointer) :void))
