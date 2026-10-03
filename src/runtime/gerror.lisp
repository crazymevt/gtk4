;;;; gerror.lisp — GError as a Lisp condition

(in-package #:gtk4.runtime)

(define-condition glib-error (error)
  ((domain :initarg :domain :reader glib-error-domain
           :documentation "The error domain's quark string, e.g. \"g-io-error-quark\".")
   (code :initarg :code :reader glib-error-code)
   (message :initarg :message :reader glib-error-message))
  (:report (lambda (c s)
             (format s "~a (~a ~a)" (glib-error-message c)
                     (glib-error-domain c) (glib-error-code c)))))

(defvar *error-domain-conditions* (make-hash-table :test 'equal)
  "Domain quark string -> condition class, so generated code can map each
GError domain (and later each code) to its own condition subclass.")

(defun signal-gerror (gerror-pointer)
  "Signal a Lisp condition for GERROR-POINTER and free the GError."
  (let* ((domain (%g-quark-to-string
                  (cffi:foreign-slot-value gerror-pointer '(:struct gerror) 'domain)))
         (code (cffi:foreign-slot-value gerror-pointer '(:struct gerror) 'code))
         (message (cffi:foreign-string-to-lisp
                   (cffi:foreign-slot-value gerror-pointer '(:struct gerror) 'message))))
    (%g-error-free gerror-pointer)
    (error (gethash domain *error-domain-conditions* 'glib-error)
           :domain domain :code code :message message)))

(defmacro with-gerror ((var) &body body)
  "Bind VAR to a GError** initialized to NULL, run BODY, and signal a
GLIB-ERROR if BODY's C call set it. Returns BODY's values otherwise."
  (let ((result (gensym "RESULT")))
    `(cffi:with-foreign-object (,var :pointer)
       (setf (cffi:mem-ref ,var :pointer) (cffi:null-pointer))
       (let ((,result (multiple-value-list (progn ,@body))))
         (let ((err (cffi:mem-ref ,var :pointer)))
           (unless (cffi:null-pointer-p err)
             (signal-gerror err)))
         (values-list ,result)))))
