;;;; full-stack.lisp — generate every target namespace into build/full/ and
;;;; time a clean compile and a cached load (the M1 compile-time gate).
;;;;
;;;; Run with `make full-stack`. Each phase runs in its own SBCL process,
;;;; because the generator must not share an image with generated code.

(push (truename ".") asdf:*central-registry*)

(defparameter *phase* (second sb-ext:*posix-argv*))
(defparameter *output* (merge-pathnames "build/full/" (truename ".")))
(defparameter *files*
  '("packages" "glib" "gobject" "gmodule" "gio" "cairo" "harfbuzz" "pango" "pango-cairo"
    "graphene" "gdk-pixbuf" "gdk" "gsk" "gtk"))

(defun seconds-since (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

(cond
  ((string= *phase* "generate")
   (ql:quickload :gtk4-generator :silent t)
   (uiop:symbol-call :gtk4.generator :generate
                     :targets (symbol-value (find-symbol "*TARGET-NAMESPACES*" :gtk4.generator))
                     :output-directory *output*))
  ((member *phase* '("compile" "load") :test #'string=)
   (ql:quickload :gtk4/runtime :silent t)
   (let ((start (get-internal-real-time)))
     (handler-bind ((warning #'muffle-warning))
       (dolist (f *files*)
         (let ((source (merge-pathnames (concatenate 'string f ".lisp") *output*)))
           (load (if (string= *phase* "compile")
                     (compile-file source :verbose nil :print nil)
                     (compile-file-pathname source))))))
     (format t "~&~a of all namespaces: ~,2f s~%" *phase* (seconds-since start))))
  (t (error "Usage: sbcl --script scripts/full-stack.lisp generate|compile|load")))
