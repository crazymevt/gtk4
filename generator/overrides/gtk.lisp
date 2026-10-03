;;;; overrides/gtk.lisp — corrections to Gtk-4.0.gir

(in-package #:gtk4.generator)

(define-override "Gtk" (ns)
  ;; The widget sets *hexpand_p and *vexpand_p.
  (mark-out-parameters ns "Widget.compute_expand" "hexpand_p" "vexpand_p"))
