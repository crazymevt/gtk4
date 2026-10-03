;;;; demos.lisp — every demo opens, runs and closes without errors

(in-package #:gtk4-tests)

(define-test demos :parent gtk4-tests)

(defun run-demo-briefly (demo &key (seconds 0.4))
  "Open DEMO's window, run the main loop for SECONDS, close it. Returns the
errors signalled in callbacks while it ran."
  (let* ((errors '())
         (rt:*callback-error-handler* (lambda (c where) (push (list where c) errors)))
         (window (gtk4-demo:make-demo-window demo)))
    (gtk:window-present window)
    (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
          while (< (get-internal-real-time) deadline)
          do (rt:iterate-main-context) (sleep 0.01))
    (gtk:window-destroy window)
    (rt:iterate-main-context)
    errors))

(define-test every-demo-runs :parent demos
  (with-gtk
    (let ((demos (gtk4-demo:all-demos)))
      (true (>= (length demos) 20) "~d demos registered" (length demos))
      (dolist (demo demos)
        (let ((errors (handler-case (run-demo-briefly demo)
                        (error (e) (list (list "building the window" e))))))
          (is = 0 (length errors) "demo ~a: ~{~{~a: ~a~}~^; ~}" (gtk4-demo:demo-name demo) errors))))))

(define-test demo-browser-opens :parent demos
  (with-gtk
    (let* ((errors '())
           (rt:*callback-error-handler* (lambda (c where) (push (list where c) errors)))
           (app (let ((app (gtk:application-new "org.lisp.gtk4.DemoTest" '(:non-unique))))
                  ;; Registering emits ::startup, after which application windows may be added.
                  (gio:application-register app nil)
                  app))
           (window (gtk4-demo::browser-window app)))
      (gtk:window-present window)
      (loop repeat 20 do (rt:iterate-main-context) (sleep 0.01))
      (gtk:window-destroy window)
      (is = 0 (length errors) "~s" errors))))
