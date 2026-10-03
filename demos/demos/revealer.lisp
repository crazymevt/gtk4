;;;; revealer.lisp — animated show and hide
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Revealer.html

(in-package #:gtk4-demo)

(define-demo revealer
    (:title "Revealer"
     :category "Animation"
     :description "GtkRevealer animates its child in and out. Pick a transition, then toggle
the button to watch it.")
  (let* ((window (make-demo-frame "Revealer" :width 380 :height 300))
         (revealer (gtk:revealer-new))
         (toggle (gtk:toggle-button-new-with-label "Reveal"))
         (choices (gtk:drop-down-new-from-strings '("crossfade" "slide-down" "slide-up" "slide-left" "slide-right")))
         (content (gtk:label-new "Now you see me")))
    (gtk:widget-add-css-class content "title-2")
    (gtk:revealer-set-child revealer (margins content 24))
    (gtk:revealer-set-transition-duration revealer 500)
    (gobject:connect toggle :toggled
                     (lambda (b) (gtk:revealer-set-reveal-child revealer (gtk:toggle-button-get-active b))))
    (gobject:connect choices "notify::selected"
                     (lambda (d p)
                       (declare (ignore p))
                       (gtk:revealer-set-transition-type
                        revealer (nth (gtk:drop-down-get-selected d)
                                      '(:crossfade :slide-down :slide-up :slide-left :slide-right)))))
    (gtk:window-set-child window (margins (vbox 12 choices toggle revealer) 24))
    window))
