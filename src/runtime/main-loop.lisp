;;;; main-loop.lisp — running work on the GUI thread

(in-package #:gtk4.runtime)

(defvar *callback-error-handler*
  (lambda (condition where)
    (format *error-output* "~&;; gtk4: error in ~a: ~a~%" where condition)
    (finish-output *error-output*))
  "Called with (CONDITION WHERE) when a Lisp callback invoked from C signals an
error. Errors never unwind through C frames. Set it to a function that calls
INVOKE-DEBUGGER to debug in place during development.")

(defmacro with-callback-protection ((where &optional default) &body body)
  "Run BODY for a callback entered from C: floats traps masked, and any error
handed to *CALLBACK-ERROR-HANDLER* instead of unwinding into C. Returns
DEFAULT when BODY fails."
  `(with-gtk-float-traps
     (handler-case (progn ,@body)
       (error (e)
         (funcall *callback-error-handler* e ,where)
         ,default))))

;;; Invoking a thunk on the main context

(cffi:defcallback invoke-thunk :int ((data :pointer))
  (with-callback-protection ("in-main-thread" 0)
    (funcall (handle-value data))
    0))                                 ; G_SOURCE_REMOVE

(defvar *gui-thread* (sb-thread:main-thread)
  "The thread that runs the GLib main loop and makes every GTK call. On macOS
it must be the initial thread; on Linux it may be set to another thread.")

(defun gui-thread-p ()
  (eq sb-thread:*current-thread* *gui-thread*))

(defvar *gui-thread-backtraces* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Condition -> the backtrace on the GUI thread where it was signalled, for
errors CALL-IN-MAIN-THREAD re-signals in the waiting thread.")

(defparameter *gui-thread-backtrace-frames* 60)

(defun gui-thread-backtrace (condition)
  "The GUI thread's backtrace, as a string, from where CONDITION was signalled,
if CONDITION is an error that CALL-IN-MAIN-THREAD with :WAIT re-signalled in
the calling thread; else NIL. The debugger shows the calling thread's stack,
which ends in the wait; this shows where the error really happened."
  (values (gethash condition *gui-thread-backtraces*)))

(defun record-gui-thread-backtrace (condition)
  (setf (gethash condition *gui-thread-backtraces*)
        (with-output-to-string (s)
          (ignore-errors (sb-debug:print-backtrace :stream s :count *gui-thread-backtrace-frames*)))))

(defun call-in-main-thread (thunk &key wait)
  "Run THUNK on the GUI thread. Called on that thread, THUNK runs at once;
from any other thread it is queued as an idle callback, run when the main
loop next iterates. With WAIT, block until it has run and return its values,
re-signalling any error in the caller; GUI-THREAD-BACKTRACE then gives the
backtrace where the error happened."
  (cond
    ((gui-thread-p)
     (funcall thunk))
    ((not wait)
     (%g-idle-add-full 0 (cffi:callback invoke-thunk) (make-handle thunk)
                       (cffi:callback free-handle-notify))
     (values))
    (t
      (let ((done (sb-thread:make-semaphore))
            (results nil)
            (failure nil))
        (call-in-main-thread
         (lambda ()
           (unwind-protect
                (handler-case
                    (handler-bind ((error #'record-gui-thread-backtrace))
                      (setf results (multiple-value-list (funcall thunk))))
                  (error (e) (setf failure e)))
             (sb-thread:signal-semaphore done))))
        (sb-thread:wait-on-semaphore done)
        (if failure
            (error failure)
            (values-list results))))))

(defmacro in-main-thread ((&key wait) &body body)
  "Run BODY on the GUI thread; see CALL-IN-MAIN-THREAD."
  `(call-in-main-thread (lambda () ,@body) :wait ,wait))

(defun iterate-main-context (&key (max 1000))
  "Dispatch pending events on the default main context without blocking.
Returns the number of iterations run. Used by tests and REPL helpers."
  (with-gtk-float-traps
    (loop for i from 0 below max
          while (%g-main-context-pending (cffi:null-pointer))
          do (%g-main-context-iteration (cffi:null-pointer) nil)
          finally (return i))))

;;; Async functions

(defmacro define-async (async finish &key callback-position finish-takes-source)
  "Record that the GAsyncReadyCallback of ASYNC (a function) is argument
CALLBACK-POSITION and that FINISH completes it; FINISH-TAKES-SOURCE says
whether FINISH takes the source object before the GAsyncResult. Used by
gio:async."
  `(setf (get ',async 'async-finish) '(,finish ,callback-position ,finish-takes-source)))

(defun async-finish-info (async)
  "(FINISH CALLBACK-POSITION FINISH-TAKES-SOURCE) for ASYNC, or NIL."
  (get async 'async-finish))
