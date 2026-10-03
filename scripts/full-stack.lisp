;;;; full-stack.lisp — time a clean compile and a cached load of everything
;;;;
;;;; Run with `make full-stack`. "compile" first deletes the cached fasls of
;;;; the generated bindings; "load" then loads them from cache. Each phase
;;;; runs in its own SBCL process.

(push (truename ".") asdf:*central-registry*)

(defparameter *phase* (second sb-ext:*posix-argv*))

(when (string= *phase* "compile")
  (dolist (fasl (directory (merge-pathnames
                            "**/*.fasl"
                            (asdf:apply-output-translations
                             (asdf:system-relative-pathname "gtk4" "src/")))))
    (delete-file fasl)))

(let ((start (get-internal-real-time)))
  (handler-bind ((warning #'muffle-warning))
    (ql:quickload :gtk4 :silent t))
  (format t "~&~a of all namespaces: ~,2f s~%" *phase*
          (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
