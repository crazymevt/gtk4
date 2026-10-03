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
  "\"ListStore\" => \"list-store\", \"IOChannel\" => \"io-channel\", \"RGBA\" => \"rgba\".
snake_case names (HarfBuzz's \"buffer_t\") become \"buffer-t\"."
  (let ((s (substitute #\- #\_ name)))
    (loop for (from . to) in *acronyms*
          do (loop for pos = (search from s)
                   while pos
                   do (setf s (concatenate 'string (subseq s 0 pos) to
                                           (subseq s (+ pos (length from)))))))
    (tidy-hyphens
     (with-output-to-string (out)
      (loop for i from 0 below (length s)
            for c = (char s i)
            for prev = (and (> i 0) (char s (1- i)))
            for next = (and (< (1+ i) (length s)) (char s (1+ i)))
            do (when (and prev (upper-case-p c)
                          (or (lower-case-p prev) (digit-char-p prev)
                              (and (upper-case-p prev) next (lower-case-p next))))
                 (write-char #\- out))
               (write-char (char-downcase c) out))))))

(defun tidy-hyphens (s)
  "Collapse runs of hyphens and trim them from the ends: \"-value--data--union\"
=> \"value-data-union\" (from GObject's private _Value__data__union)."
  (let ((out (with-output-to-string (o)
               (loop for c across s
                     for prev = nil then last
                     for last = c
                     unless (and (char= c #\-) (eql prev #\-))
                       do (write-char c o)))))
    (string-trim "-" out)))

(defun snake-to-kebab (name)
  (substitute #\- #\_ (string-downcase name)))

(defun constant-lisp-name (name)
  (format nil "+~a+" (snake-to-kebab name)))

(defun cl-symbol-p (name)
  (multiple-value-bind (sym status) (find-symbol (string-upcase name) "COMMON-LISP")
    (declare (ignore sym))
    (eq status :external)))

(defun safe-variable-name (name)
  "Parameter names that are CL constants, special variables or special
operators (t, pi, function) get a suffix: they cannot be bound, or print as
#'... in generated code."
  (let ((name (snake-to-kebab name)))
    (multiple-value-bind (sym status) (find-symbol (string-upcase name) "COMMON-LISP")
      (if (and (eq status :external)
               (or (boundp sym) (constantp sym) (special-operator-p sym)))
          (concatenate 'string name "-value")
          name))))

(defparameter *runtime-exports*
  '(("GObject" "CONNECT" "DISCONNECT" "EMIT" "BLOCK-HANDLER" "UNBLOCK-HANDLER"
     "HANDLER-CONNECTED-P" "PROPERTY" "OBJECT-POINTER" "GOBJECT-CLASS" "CLASS-GTYPE")
    ("GLib" "GLIB-ERROR" "GLIB-ERROR-DOMAIN" "GLIB-ERROR-CODE" "GLIB-ERROR-MESSAGE"
     "IN-MAIN-THREAD" "CALL-IN-MAIN-THREAD" "MAIN-THREAD-P" "WITH-GTK-FLOAT-TRAPS"))
  "Runtime symbols each namespace's package re-exports, so users write
gobject:connect and glib:in-main-thread rather than naming the runtime.")

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
