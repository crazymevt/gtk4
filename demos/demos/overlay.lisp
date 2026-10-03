;;;; overlay.lisp — widgets stacked on top of each other
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Overlay.html

(in-package #:gtk4-demo)

(define-demo overlay
    (:title "Overlay"
     :category "Layout"
     :description "A GtkOverlay draws a badge and a floating button over a large text view.
The overlay children keep their place as the window resizes.")
  (let* ((window (make-demo-frame "Overlay" :width 480 :height 320))
         (overlay (gtk:overlay-new))
         (text (gtk:text-view-new))
         (badge (gtk:label-new "Draft"))
         (button (gtk:button-new-with-label "Clear")))
    (gtk:text-buffer-set-text (gtk:text-view-get-buffer text)
                              "Type here. The badge and the button float above this text." -1)
    (gtk:text-view-set-wrap-mode text :word)
    (gtk:overlay-set-child overlay (scrolled text))
    (gtk:widget-set-halign badge :end)
    (gtk:widget-set-valign badge :start)
    (gtk:widget-add-css-class badge "title-4")
    (gtk:overlay-add-overlay overlay (margins badge 12))
    (gtk:widget-set-halign button :end)
    (gtk:widget-set-valign button :end)
    (gtk:widget-add-css-class button "suggested-action")
    (gobject:connect button :clicked
                     (lambda (b) (declare (ignore b))
                       (gtk:text-buffer-set-text (gtk:text-view-get-buffer text) "" -1)))
    (gtk:overlay-add-overlay overlay (margins button 12))
    (gtk:window-set-child window overlay)
    window))
