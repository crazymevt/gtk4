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
