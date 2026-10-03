;;;; async.lisp — calling GIO-style async functions with one form
;;;;
;;;; C splits an asynchronous operation in two: foo_async starts it and takes
;;;; a GAsyncReadyCallback, and foo_finish, called from that callback, returns
;;;; the result or the error. GIO:ASYNC joins them:
;;;;
;;;;   (gio:async (gio:file-load-contents-async file nil)
;;;;              (lambda (contents etag) (declare (ignore etag)) (show contents))
;;;;              :error (lambda (e) (warn "~a" e)))

(in-package #:gtk4)

(defmacro gio:async ((function &rest args) on-success &key error)
  "Start the asynchronous operation (FUNCTION ARGS...), omitting its callback
argument (and any optional arguments before it, which default to NIL). When
it completes, call ON-SUCCESS with the values of FUNCTION's _finish function;
if that signals a GLIB-ERROR, call ERROR with the condition instead (with no
ERROR function, the error goes to the callback error handler)."
  (let ((info (gtk4.runtime:async-finish-info function)))
    (unless info
      (error "gio:async: ~s is not an async function with a known _finish function" function))
    (destructuring-bind (finish position takes-source) info
      (when (> (length args) position)
        (error "gio:async: ~s takes ~d argument~:p before its callback, not ~d"
               function position (length args)))
      (let ((source (gensym "SOURCE")) (result (gensym "RESULT")) (e (gensym "E")) (done (gensym "DONE")))
        `(,function ,@args ,@(make-list (- position (length args)))
                    (lambda (,source ,result)
                      (declare (ignorable ,source))
                      (block ,done
                        (multiple-value-call ,on-success
                          (handler-case (,finish ,@(when takes-source (list source)) ,result)
                            (glib:glib-error (,e)
                              (return-from ,done ,(if error `(funcall ,error ,e) `(error ,e)))))))))))))
