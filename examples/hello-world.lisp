;;;; hello-world.lisp — a window with a button, using the generated bindings
;;;;
;;;; Follows the first example of GTK's "Getting Started" guide:
;;;; https://docs.gtk.org/gtk4/getting_started.html
;;;;
;;;; Run from a shell (macOS needs the main thread, which a script has):
;;;;   make hello
;;;; or with automatic quit after 3 seconds:
;;;;   make hello QUIT_AFTER=3

(defpackage #:gtk4-examples.hello-world
  (:use #:cl)
  (:export #:main))

(in-package #:gtk4-examples.hello-world)

(defun activate (app)
  (let ((window (gtk:application-window-new app))
        (button (gtk:button-new-with-label "Say hello")))
    (gtk:window-set-title window "Hello, GTK 4 from SBCL")
    (gtk:window-set-default-size window 360 200)
    (gobject:connect button :clicked
                     (lambda (button)
                       (declare (ignore button))
                       (format t "~&Hello from Lisp!~%")
                       (finish-output)))
    (gtk:window-set-child window button)
    (gtk:window-present window)))

(defun main (&key quit-after)
  "Open a window with one button. With QUIT-AFTER (seconds), quit automatically."
  (unless (glib:main-thread-p)
    (error "GTK must run on the main thread; start this from a script."))
  (let ((app (gtk:application-new "org.lisp.gtk4.HelloWorld" '(:default-flags))))
    (gobject:connect app :activate #'activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (glib:with-gtk-float-traps
      (gio:application-run app nil))))
