;;;; overrides/gdk.lisp — corrections to Gdk-4.0.gir

(in-package #:gtk4.generator)

(define-override "Gdk" (ns)
  ;; A plain C function taking a string; GIR marks it non-introspectable
  ;; because language bindings can use gdk_clipboard_set_value instead.
  (mark-introspectable ns "gdk_clipboard_set_text"))
