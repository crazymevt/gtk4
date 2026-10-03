;;;; libraries.lisp — locating and loading the GTK stack's shared libraries

(in-package #:gtk4.runtime)

(alexandria:define-constant +minimum-gtk-version+ '(4 14 0)
  :test #'equal
  :documentation "Oldest GTK release these bindings support.")

(defvar *library-directories*
  (append
   (let ((env (uiop:getenv "GTK4_LISP_LIBRARY_PATH")))
     (when (and env (plusp (length env)))
       (mapcar #'uiop:ensure-directory-pathname
               (uiop:split-string env :separator (string (uiop:inter-directory-separator))))))
   #+darwin '(#p"/opt/homebrew/lib/" #p"/usr/local/lib/" #p"/opt/local/lib/"))
  "Extra directories searched for the GTK shared libraries, before the system defaults.
Initialized from the GTK4_LISP_LIBRARY_PATH environment variable.")

(defmacro define-gtk-library (name &key darwin linux windows)
  `(cffi:define-foreign-library ,name
     (:darwin ,darwin)
     (:windows ,windows)
     (:unix ,linux)))

;;; Listed in dependency order; LOAD-LIBRARIES loads them in this order.
(define-gtk-library glib
  :darwin "libglib-2.0.0.dylib" :linux "libglib-2.0.so.0" :windows "libglib-2.0-0.dll")
(define-gtk-library gobject
  :darwin "libgobject-2.0.0.dylib" :linux "libgobject-2.0.so.0" :windows "libgobject-2.0-0.dll")
(define-gtk-library gmodule
  :darwin "libgmodule-2.0.0.dylib" :linux "libgmodule-2.0.so.0" :windows "libgmodule-2.0-0.dll")
(define-gtk-library gio
  :darwin "libgio-2.0.0.dylib" :linux "libgio-2.0.so.0" :windows "libgio-2.0-0.dll")
(define-gtk-library cairo
  :darwin "libcairo.2.dylib" :linux "libcairo.so.2" :windows "libcairo-2.dll")
(define-gtk-library harfbuzz
  :darwin "libharfbuzz.0.dylib" :linux "libharfbuzz.so.0" :windows "libharfbuzz-0.dll")
(define-gtk-library pango
  :darwin "libpango-1.0.0.dylib" :linux "libpango-1.0.so.0" :windows "libpango-1.0-0.dll")
(define-gtk-library pangocairo
  :darwin "libpangocairo-1.0.0.dylib" :linux "libpangocairo-1.0.so.0" :windows "libpangocairo-1.0-0.dll")
(define-gtk-library graphene
  :darwin "libgraphene-1.0.0.dylib" :linux "libgraphene-1.0.so.0" :windows "libgraphene-1.0-0.dll")
(define-gtk-library gdk-pixbuf
  :darwin "libgdk_pixbuf-2.0.0.dylib" :linux "libgdk_pixbuf-2.0.so.0" :windows "libgdk_pixbuf-2.0-0.dll")
(define-gtk-library gtk
  :darwin "libgtk-4.1.dylib" :linux "libgtk-4.so.1" :windows "libgtk-4-1.dll")
(define-gtk-library adwaita
  :darwin "libadwaita-1.0.dylib" :linux "libadwaita-1.so.0" :windows "libadwaita-1-0.dll")

(defparameter *core-libraries*
  '(glib gobject gmodule gio cairo harfbuzz pango pangocairo graphene gdk-pixbuf gtk)
  "Libraries every gtk4 program needs, in load order.")

(defun library-loaded-p (name)
  (let ((lib (cffi::get-foreign-library name)))
    (and lib (cffi:foreign-library-loaded-p lib))))

(defun load-libraries (&key (libraries *core-libraries*))
  "Load LIBRARIES (symbols naming foreign libraries in this package), skipping any
already loaded. Signals an error naming the library and the directories searched
when one cannot be found."
  (let ((cffi:*foreign-library-directories*
          (append *library-directories* cffi:*foreign-library-directories*)))
    (dolist (name libraries)
      (unless (library-loaded-p name)
        (handler-case (cffi:load-foreign-library name)
          (cffi:load-foreign-library-error (e)
            (error "gtk4: could not load the ~(~a~) library.~%~
                    Searched ~{~a~^, ~} and the system defaults.~%~
                    Set GTK4_LISP_LIBRARY_PATH to the directory holding it.~%~a"
                   name (mapcar #'namestring *library-directories*) e)))))))

(defun gtk-version ()
  "The running GTK version as a list (MAJOR MINOR MICRO)."
  (list (cffi:foreign-funcall "gtk_get_major_version" :uint)
        (cffi:foreign-funcall "gtk_get_minor_version" :uint)
        (cffi:foreign-funcall "gtk_get_micro_version" :uint)))

(defun gtk-version>= (major &optional (minor 0) (micro 0))
  "True when the running GTK is at least MAJOR.MINOR.MICRO."
  (destructuring-bind (ma mi mc) (gtk-version)
    (or (> ma major)
        (and (= ma major) (or (> mi minor)
                              (and (= mi minor) (>= mc micro)))))))

(defun check-gtk-version ()
  (unless (apply #'gtk-version>= +minimum-gtk-version+)
    (warn "gtk4: GTK ~{~a~^.~} is older than the supported minimum ~{~a~^.~}."
          (gtk-version) +minimum-gtk-version+)))

(load-libraries)
(check-gtk-version)
