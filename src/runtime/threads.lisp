;;;; threads.lisp — float traps and the GUI thread

(in-package #:gtk4.runtime)

(defmacro with-gtk-float-traps (&body body)
  "Run BODY with the floating-point traps masked that GTK, Cairo and graphics
drivers routinely trip. SBCL enables them by default; leaving them on turns
harmless C arithmetic into Lisp errors or crashes."
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body))

(defun main-thread-p ()
  "True when called on the process's initial thread. macOS only lets this
thread create windows."
  (eq sb-thread:*current-thread* (sb-thread:main-thread)))

(define-condition wrong-thread-error (error)
  ((operation :initarg :operation :reader wrong-thread-operation))
  (:report (lambda (c s)
             (format s "~a must run on the main thread, but was called from ~a.~%~
                        From a REPL worker thread, start GTK from the main thread ~
                        (see the manual's \"Threads and macOS\" chapter)."
                     (wrong-thread-operation c) sb-thread:*current-thread*))))

(defun check-main-thread (operation)
  "Signal WRONG-THREAD-ERROR unless running on the main thread. Only enforced
on macOS, where Cocoa requires it; Linux accepts any single GUI thread."
  #+darwin (unless (main-thread-p)
             (error 'wrong-thread-error :operation operation))
  #-darwin (declare (ignore operation))
  (values))
