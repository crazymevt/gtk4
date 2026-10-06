;;;; runtime.lisp — runtime tests

(in-package #:gtk4-tests)

(define-test runtime :parent gtk4-tests)

(define-test libraries-loaded :parent runtime
  (dolist (lib '(rt::glib rt::gobject rt::gio rt::gtk))
    (true (rt:library-loaded-p lib) "~a loaded" lib)))

(define-test gtk-version :parent runtime
  (let ((v (rt:gtk-version)))
    (is = 3 (length v))
    (is = 4 (first v))
    (true (rt:gtk-version>= 4 0))
    (false (rt:gtk-version>= 5 0))))

(define-test float-traps-masked :parent runtime
  ;; Inside the macro, a float division by zero yields infinity instead of an error.
  (let ((zero 0d0))
    (true (sb-ext:float-infinity-p (rt:with-gtk-float-traps (/ 1d0 zero))))))

(define-test gui-only-functions :parent runtime
  (dolist (name '("gtk_widget_show" "gdk_display_get_default" "adw_dialog_present"
                  "gsk_renderer_realize"))
    (true (rt::gui-only-function-p name) "~a is GUI-only" name))
  (dolist (name '("g_idle_add_full" "g_file_new_for_path" "gdk_keyval_to_unicode"
                  "gdk_rgba_parse" "gdk_texture_new_from_bytes" "gtk_get_major_version"
                  "gtk_widget_get_type" "gsk_rounded_rect_init"))
    (false (rt::gui-only-function-p name) "~a is not GUI-only" name)))

(define-test wrong-thread-calls-reported :parent runtime
  (let ((rt:*check-gui-thread* t)
        (rt::*wrong-thread-calls* (make-hash-table :test 'equal :synchronized t))
        (reports '())
        (*error-output* (make-broadcast-stream)))
    (let ((rt:*wrong-thread-hook* (lambda (c-name thread) (push (list c-name thread) reports))))
      ;; On the GUI thread: nothing.
      (rt::check-gui-call "gtk_test_function")
      (is equal '() reports)
      ;; From another thread: reported once, however often it happens.
      (let ((hook rt:*wrong-thread-hook*) (calls rt::*wrong-thread-calls*))
        (sb-thread:join-thread
         (sb-thread:make-thread
          (lambda ()
            (let ((rt:*check-gui-thread* t) (rt:*wrong-thread-hook* hook)
                  (rt::*wrong-thread-calls* calls) (*error-output* (make-broadcast-stream)))
              (dotimes (i 3) (rt::check-gui-call "gtk_test_function"))))
          :name "worker")))
      (is equal '(("gtk_test_function" "worker")) reports)
      (is = 3 (gethash "gtk_test_function" rt::*wrong-thread-calls*)))))

(define-test wrong-thread-check-off-by-default :parent runtime
  (false rt:*check-gui-thread*))
