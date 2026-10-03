;;;; image.lisp — saving the bindings in an executable
;;;;
;;;; A saved image starts as a new process: C function addresses, GType ids,
;;;; objects and the main thread are all different. Before saving, the
;;;; runtime forgets everything that was only valid in the old process;
;;;; when the image starts, it loads the libraries again (from beside the
;;;; executable, if they were bundled) and looks things up as it needs them.

(in-package #:gtk4.runtime)

(defvar *loaded-libraries* '()
  "Foreign libraries loaded when the image was saved, to load again at startup.")

(defun forget-process-state ()
  "Drop state that is only valid in this process; on SB-EXT:*SAVE-HOOKS*."
  (when (plusp (hash-table-count *proxies*))
    (warn "gtk4: saving an image with ~d live GObject proxies. Objects do not ~
           survive into the saved program; create them when it starts."
          (hash-table-count *proxies*)))
  (dolist (table (list *proxies* *strong-proxies* *toggled* *handles* *gtype-classes*
                       *lisp-gtypes* *enum-converters*))
    (clrhash table))
  (setf *release-queue* nil
        *release-scheduled* nil
        *strv-gtype* nil
        *g-type-gtype* nil)
  (dolist (cell *fcells*) (setf (fcell-pointer cell) nil))
  (dolist (cell *gtype-cells*) (setf (gtype-cell-value cell) nil))
  (loop for class being the hash-values of *gtype-name-classes*
        do (setf (slot-value class 'gtype) nil
                 (class-registered-shape class) nil))
  (loop for (name) in *enum-gtypes*
        do (setf (enum-info-gtype (enum-info name)) nil))
  ;; Entry points into Lisp are made again on first use.
  (loop for info being the hash-values of *vfuncs*
        do (setf (vfunc-info-trampoline info) nil))
  ;; Close the libraries so the saved program finds them where it runs.
  (setf *loaded-libraries*
        (remove-if-not #'library-loaded-p
                       (append *core-libraries* '(adwaita))))
  (dolist (name (reverse *loaded-libraries*))
    (cffi:close-foreign-library name)))

(defun use-bundled-data ()
  "When the program runs from a bundle with GTK's data in Resources/share
(see scripts/macos-app.sh), point GLib and GTK at it."
  (let* ((exe (and sb-ext:*runtime-pathname*
                   (uiop:pathname-directory-pathname sb-ext:*runtime-pathname*)))
         (share (and exe (probe-file (merge-pathnames "../Resources/share/" exe)))))
    (when share
      (flet ((setenv (name value)
               (cffi:foreign-funcall "setenv" :string name :string value :int 1 :int)))
        (let ((dir (namestring share))
              (old (uiop:getenv "XDG_DATA_DIRS")))
          (setenv "XDG_DATA_DIRS" (if (and old (plusp (length old)))
                                      (format nil "~a:~a" dir old)
                                      dir))
          (setenv "GSETTINGS_SCHEMA_DIR" (format nil "~aglib-2.0/schemas" dir)))))))

(defun restore-process-state ()
  "Load the libraries and register enum GTypes again; on SB-EXT:*INIT-HOOKS*."
  (use-bundled-data)
  (setf *gui-thread* (sb-thread:main-thread)
        *library-directories* (default-library-directories))
  (load-libraries :libraries *loaded-libraries*)
  (loop for (name gtype-name get-type) in *enum-gtypes*
        do (register-enum-gtype name gtype-name get-type)))

(pushnew 'forget-process-state sb-ext:*save-hooks*)
(pushnew 'restore-process-state sb-ext:*init-hooks*)
