;;;; overrides/glib.lisp — corrections to GLib-2.0.gir

(in-package #:gtk4.generator)

(define-override "GLib" (ns)
  ;; "return location for the fractional part of seconds elapsed".
  (mark-out-parameters ns "g_timer_elapsed" "microseconds"))
