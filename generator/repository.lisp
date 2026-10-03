;;;; repository.lisp — finding GIR files and loading a namespace with its includes

(in-package #:gtk4.generator)

(defun default-gir-search-path ()
  (remove-duplicates
   (append
    (let ((env (uiop:getenv "GI_GIR_PATH")))
      (and env (plusp (length env))
           (mapcar #'uiop:ensure-directory-pathname
                   (uiop:split-string env :separator ":"))))
    (let ((env (uiop:getenv "XDG_DATA_DIRS")))
      (and env (plusp (length env))
           (mapcar (lambda (d) (merge-pathnames "gir-1.0/" (uiop:ensure-directory-pathname d)))
                   (uiop:split-string env :separator ":"))))
    (list (asdf:system-relative-pathname "gtk4-generator" "gir/")
          #p"/opt/homebrew/share/gir-1.0/"
          #p"/usr/local/share/gir-1.0/"
          #p"/usr/share/gir-1.0/"))
   :test #'equal :from-end t))

(defvar *gir-search-path* (default-gir-search-path)
  "Directories searched, in order, for NAME-VERSION.gir files.
Includes GI_GIR_PATH, XDG_DATA_DIRS/gir-1.0, the project's gir/ directory
and the usual system locations.")

(defparameter *target-namespaces*
  '(("GLib" "2.0") ("GObject" "2.0") ("GModule" "2.0") ("Gio" "2.0")
    ("cairo" "1.0") ("HarfBuzz" "0.0") ("Pango" "1.0") ("PangoCairo" "1.0")
    ("Graphene" "1.0") ("GdkPixbuf" "2.0")
    ("Gdk" "4.0") ("Gsk" "4.0") ("Gtk" "4.0"))
  "Namespaces the gtk4 system binds. libadwaita (\"Adw\" \"1\") is a separate target.")

(defun find-gir-file (name version)
  (let ((file (format nil "~a-~a.gir" name version)))
    (loop for dir in *gir-search-path*
          for path = (merge-pathnames file dir)
          when (probe-file path) return it)))

(define-condition missing-gir (warning)
  ((name :initarg :name) (version :initarg :version))
  (:report (lambda (c s)
             (with-slots (name version) c
               (format s "No ~a-~a.gir found on the GIR search path." name version)))))

(defun repository-key (name version)
  (format nil "~a-~a" name version))

(defun load-repository (name version &optional (table (make-hash-table :test 'equal)))
  "Load NAME-VERSION and, recursively, every namespace it includes into TABLE,
keyed by \"Name-Version\". A missing file signals a MISSING-GIR warning and is
recorded as :MISSING. Returns TABLE."
  (let ((key (repository-key name version)))
    (unless (nth-value 1 (gethash key table))
      (let ((path (find-gir-file name version)))
        (cond
          ((null path)
           (warn 'missing-gir :name name :version version)
           (setf (gethash key table) :missing))
          (t
           (let ((ns (parse-gir-file path)))
             (setf (gethash key table) ns)
             (loop for (iname iversion) in (gir-namespace-includes ns)
                   do (load-repository iname iversion table)))))))
    table))

(defun load-targets (&key (targets *target-namespaces*))
  "Load every namespace in TARGETS (and their includes) into one table."
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name version) in targets
          do (load-repository name version table))
    table))
