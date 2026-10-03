;;;; hello-world.lisp — a window with a button
;;;; GTK docs: https://docs.gtk.org/gtk4/getting_started.html

(in-package #:gtk4-demo)

(define-demo hello-world
    (:title "Hello world"
     :category "Basics"
     :description "A window holding one button. Clicking it changes its label: the smallest
program that reacts to the user.")
  (let ((window (make-demo-frame "Hello world" :width 320 :height 200))
        (button (gtk:button-new-with-label "Click me"))
        (clicks 0))
    (gobject:connect button :clicked
                     (lambda (button)
                       (gtk:button-set-label button (format nil "Clicked ~d time~:p" (incf clicks)))))
    (gtk:widget-set-halign button :center)
    (gtk:widget-set-valign button :center)
    (gtk:window-set-child window button)
    window))
