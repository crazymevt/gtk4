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
    (error (error-condition-class domain code) :domain domain :code code :message message)))

;;; Error domains: the generated bindings define a condition class for each
;;; GError domain (gio:io-error) and each of its codes (gio:io-error-not-found).

(defvar *error-domains* (make-hash-table :test 'equal)
  "Domain quark string -> (CONDITION ENUM CODE-TABLE), CODE-TABLE mapping
each code to its condition class.")

(defun error-condition-class (domain code)
  (let ((d (gethash domain *error-domains*)))
    (cond (d (gethash code (third d) (first d)))
          (t (gethash domain *error-domain-conditions* 'glib-error)))))

(defun register-error-domain (quark condition enum codes)
  (let ((by-code (make-hash-table)))
    (loop for (key code-condition) in codes
          do (setf (gethash (enum-value enum key) by-code) code-condition))
    (setf (gethash quark *error-domains*) (list condition enum by-code)
          (gethash quark *error-domain-conditions*) condition)))

(defmacro define-gerror-domain (name (quark enum) &body codes)
  "Define condition NAME for GError domain QUARK, whose codes are members of
ENUM, and a subclass of NAME for each (KEYWORD CONDITION-NAME) in CODES."
  `(progn
     (define-condition ,name (glib-error) ()
       (:documentation ,(format nil "A GError in domain ~a; the code is a ~(~a~)." quark enum)))
     ,@(loop for (key code-condition) in codes
             collect `(define-condition ,code-condition (,name) ()
                        (:documentation ,(format nil "A GError in domain ~a with code ~s." quark key))))
     (register-error-domain ,quark ',name ',enum ',codes)
     ',name))

(defun glib-error-keyword (condition)
  "CONDITION's error code as a keyword of its domain's enum (:not-found), or
the integer code for a domain without one."
  (let ((d (gethash (glib-error-domain condition) *error-domains*)))
    (if d
        (enum-keyword (second d) (glib-error-code condition))
        (glib-error-code condition))))

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

(defun set-gerror-from-condition (error-location condition)
  "Store CONDITION in the GError** ERROR-LOCATION (when not NULL), for a Lisp
implementation of a C function that reports errors. A GLIB-ERROR keeps its
domain and code; any other error becomes gtk4-lisp-error-quark code 0."
  (unless (cffi:null-pointer-p error-location)
    (multiple-value-bind (domain code)
        (if (typep condition 'glib-error)
            (values (glib-error-domain condition) (glib-error-code condition))
            (values "gtk4-lisp-error-quark" 0))
      (cffi:foreign-funcall "g_set_error_literal"
                            :pointer error-location
                            :uint32 (cffi:foreign-funcall "g_quark_from_string" :string domain :uint32)
                            :int code
                            :string (if (typep condition 'glib-error)
                                        (glib-error-message condition)
                                        (princ-to-string condition))
                            :void))))
