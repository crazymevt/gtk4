;;;; hello-world.lisp — M0 hello world, written against raw CFFI
;;;;
;;;; This predates the generated bindings: it calls the C API directly to prove
;;;; the runtime can load GTK, run its main loop and receive callbacks. Once the
;;;; generator emits bindings it will be rewritten as
;;;;   (make-instance 'gtk:application-window :title "Hello" ...)
;;;;
;;;; Run from a shell (macOS needs the main thread, which a script has):
;;;;   make hello
;;;; or with automatic quit after 3 seconds:
;;;;   make hello QUIT_AFTER=3

(defpackage #:gtk4-examples.hello-world
  (:use #:cl)
  (:export #:main))

(in-package #:gtk4-examples.hello-world)

(cffi:defcfun "gtk_application_new" :pointer (id :string) (flags :int))
(cffi:defcfun "g_application_run" :int (app :pointer) (argc :int) (argv :pointer))
(cffi:defcfun "g_application_quit" :void (app :pointer))
(cffi:defcfun "g_object_unref" :void (object :pointer))
(cffi:defcfun "g_signal_connect_data" :ulong
  (instance :pointer) (signal :string) (handler :pointer)
  (data :pointer) (destroy :pointer) (flags :int))
(cffi:defcfun "g_timeout_add_seconds" :uint (seconds :uint) (fn :pointer) (data :pointer))
(cffi:defcfun "gtk_application_window_new" :pointer (app :pointer))
(cffi:defcfun "gtk_window_set_title" :void (window :pointer) (title :string))
(cffi:defcfun "gtk_window_set_default_size" :void (window :pointer) (w :int) (h :int))
(cffi:defcfun "gtk_window_set_child" :void (window :pointer) (child :pointer))
(cffi:defcfun "gtk_window_present" :void (window :pointer))
(cffi:defcfun "gtk_button_new_with_label" :pointer (label :string))

(defvar *app* (cffi:null-pointer))

(cffi:defcallback on-clicked :void ((button :pointer) (data :pointer))
  (declare (ignore button data))
  (format t "~&Hello from Lisp!~%")
  (finish-output))

(cffi:defcallback on-quit-timer :int ((data :pointer))
  (declare (ignore data))
  (g-application-quit *app*)
  0)                                    ; G_SOURCE_REMOVE

(cffi:defcallback on-activate :void ((app :pointer) (data :pointer))
  (declare (ignore data))
  (let ((window (gtk-application-window-new app))
        (button (gtk-button-new-with-label "Say hello")))
    (gtk-window-set-title window "Hello, GTK 4 from SBCL")
    (gtk-window-set-default-size window 360 200)
    (g-signal-connect-data button "clicked" (cffi:callback on-clicked)
                           (cffi:null-pointer) (cffi:null-pointer) 0)
    (gtk-window-set-child window button)
    (gtk-window-present window)))

(defun main (&key quit-after)
  "Open a window with one button. With QUIT-AFTER (seconds), quit automatically."
  (gtk4.runtime:check-main-thread 'main)
  (gtk4.runtime:with-gtk-float-traps
    (let ((app (gtk-application-new "org.lisp.gtk4.HelloWorld" 0)))
      (setf *app* app)
      (g-signal-connect-data app "activate" (cffi:callback on-activate)
                             (cffi:null-pointer) (cffi:null-pointer) 0)
      (when quit-after
        (g-timeout-add-seconds quit-after (cffi:callback on-quit-timer) (cffi:null-pointer)))
      (unwind-protect (g-application-run app 0 (cffi:null-pointer))
        (g-object-unref app)
        (setf *app* (cffi:null-pointer))))))
