;;;; deploy.lisp — standalone executables
;;;;
;;;;   sbcl --non-interactive --eval '(ql:quickload :my-app)' \
;;;;        --eval '(gtk4:save-executable "my-app" (lambda () (my-app:main)))'
;;;;
;;;; The executable contains Lisp and the bindings, not GTK itself: it loads
;;;; the GTK libraries when it starts, from GTK4_LISP_LIBRARY_PATH, lib/
;;;; beside the executable, a macOS bundle's Frameworks/, or the system.

(in-package #:gtk4)

(defun save-executable (path main &key (compression nil))
  "Save the running Lisp as an executable at PATH that calls MAIN (a
function of no arguments) and exits with its value when an integer, else 0.
Call it from a script, in a Lisp that has loaded the program but not yet
created any GTK objects; the Lisp exits. COMPRESSION is passed to
SB-EXT:SAVE-LISP-AND-DIE (when this SBCL supports it)."
  (when (rest (sb-thread:list-all-threads))
    (error "gtk4:save-executable: other threads are running (~{~a~^, ~}); ~
            save from a script run with --non-interactive"
           (mapcar #'sb-thread:thread-name (rest (sb-thread:list-all-threads)))))
  ;; A failed save rolls the image back mid-way; avoid the common cause.
  (ensure-directories-exist path)
  (apply #'sb-ext:save-lisp-and-die path
         :executable t
         :save-runtime-options nil
         :toplevel (lambda ()
                     (let ((status (glib:with-gtk-float-traps (funcall main))))
                       (sb-ext:exit :code (if (integerp status) status 0) :abort nil)))
         (when compression (list :compression compression))))
