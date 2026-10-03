;;;; examples.lisp — every example program runs
;;;;
;;;; Each examples/NAME.lisp defines package gtk4-examples.NAME with a MAIN
;;;; that accepts :quit-after. Runs each for a second; any Lisp error in a
;;;; callback fails the test.

(in-package #:gtk4-tests)

(define-test examples :parent gtk4-tests)

(defun example-files ()
  (directory (merge-pathnames "*.lisp" (asdf:system-relative-pathname "gtk4" "examples/"))))

(define-test every-example-runs :parent examples
  (with-gtk
    (dolist (file (example-files))
      (let ((errors '()))
        (let ((rt:*callback-error-handler* (lambda (e where) (push (list where e) errors))))
          (load file)
          (uiop:symbol-call (format nil "GTK4-EXAMPLES.~:@(~a~)" (pathname-name file)) "MAIN"
                            :quit-after 1))
        (is equal '() errors "~a: ~s" (pathname-name file) errors)))))
