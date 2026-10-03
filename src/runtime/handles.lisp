;;;; handles.lisp — passing Lisp objects through C as small integers
;;;;
;;;; C never holds a pointer to a Lisp object (the GC moves them). Instead a
;;;; Lisp value is stored here under an integer handle, the handle travels as
;;;; a gpointer user_data, and a GDestroyNotify releases it.

(in-package #:gtk4.runtime)

(defvar *handles* (make-hash-table))
(defvar *handles-lock* (sb-thread:make-mutex :name "gtk4 handles"))
(defvar *next-handle* 0)

(defun make-handle (value)
  "Store VALUE and return a non-null foreign pointer encoding its handle."
  (sb-thread:with-mutex (*handles-lock*)
    (let ((id (incf *next-handle*)))
      (setf (gethash id *handles*) value)
      (cffi:make-pointer id))))

(defun handle-value (pointer)
  (sb-thread:with-mutex (*handles-lock*)
    (gethash (cffi:pointer-address pointer) *handles*)))

(defun free-handle (pointer)
  (sb-thread:with-mutex (*handles-lock*)
    (remhash (cffi:pointer-address pointer) *handles*)))

(defun handle-count ()
  "Number of live handles; tests use it to check nothing leaks."
  (sb-thread:with-mutex (*handles-lock*)
    (hash-table-count *handles*)))

(cffi:defcallback free-handle-notify :void ((data :pointer))
  (free-handle data))

(cffi:defcallback free-handle-closure-notify :void ((data :pointer) (closure :pointer))
  (declare (ignore closure))
  (free-handle data))
