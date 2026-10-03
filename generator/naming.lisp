;;;; naming.lisp — GIR names to Lisp names, packages and symbols

(in-package #:gtk4.generator)

(defparameter *namespace-packages*
  '(("GLib" . "GLIB") ("GObject" . "GOBJECT") ("GModule" . "GMODULE") ("Gio" . "GIO")
    ("cairo" . "CAIRO") ("HarfBuzz" . "HARFBUZZ") ("freetype2" . "FREETYPE2")
    ("Pango" . "PANGO") ("PangoCairo" . "PANGO-CAIRO") ("Graphene" . "GRAPHENE")
    ("GdkPixbuf" . "GDK-PIXBUF") ("Gdk" . "GDK") ("Gsk" . "GSK") ("Gtk" . "GTK")
    ("Adw" . "ADW"))
  "GIR namespace -> Lisp package name.")

(defun namespace-package-name (namespace)
  (or (cdr (assoc namespace *namespace-packages* :test #'string=))
      (string-upcase namespace)))

(defparameter *acronyms* '(("DBus" . "Dbus"))
  "Words rewritten before splitting CamelCase, so DBusProxy is dbus-proxy.")

(defun camel-to-kebab (name)
  "\"ListStore\" => \"list-store\", \"IOChannel\" => \"io-channel\", \"RGBA\" => \"rgba\"."
  (let ((s name))
    (loop for (from . to) in *acronyms*
          do (loop for pos = (search from s)
                   while pos
                   do (setf s (concatenate 'string (subseq s 0 pos) to
                                           (subseq s (+ pos (length from)))))))
    (with-output-to-string (out)
      (loop for i from 0 below (length s)
            for c = (char s i)
            for prev = (and (> i 0) (char s (1- i)))
            for next = (and (< (1+ i) (length s)) (char s (1+ i)))
            do (when (and prev (upper-case-p c)
                          (or (lower-case-p prev) (digit-char-p prev)
                              (and (upper-case-p prev) next (lower-case-p next))))
                 (write-char #\- out))
               (write-char (char-downcase c) out)))))

(defun snake-to-kebab (name)
  (substitute #\- #\_ (string-downcase name)))

(defun constant-lisp-name (name)
  (format nil "+~a+" (snake-to-kebab name)))

(defun cl-symbol-p (name)
  (multiple-value-bind (sym status) (find-symbol (string-upcase name) "COMMON-LISP")
    (declare (ignore sym))
    (eq status :external)))

(defun safe-variable-name (name)
  "Parameter names that are CL constants or special variables (t, pi) get a suffix."
  (let ((name (snake-to-kebab name)))
    (multiple-value-bind (sym status) (find-symbol (string-upcase name) "COMMON-LISP")
      (if (and (eq status :external) (or (boundp sym) (constantp sym)))
          (concatenate 'string name "-value")
          name))))

;;; Type symbols for qualified GIR names ("Gtk.Widget")

(defvar *type-symbols* (make-hash-table :test 'equal)
  "Qualified GIR type name -> Lisp symbol naming it.")

(defparameter *runtime-types*
  '(("GObject.Object" . "OBJECT") ("GObject.InitiallyUnowned" . "INITIALLY-UNOWNED"))
  "GIR types whose Lisp class is defined by the runtime, not generated.")

(defun qualify (name namespace)
  (if (find #\. name) name (format nil "~a.~a" namespace name)))

(defun type-symbol (qualified-name)
  (gethash qualified-name *type-symbols*))
