;;;; stack-sidebar.lisp — pages switched from a sidebar
;;;; GTK docs: https://docs.gtk.org/gtk4/class.StackSidebar.html

(in-package #:gtk4-demo)

(define-demo stack-sidebar
    (:title "Stack and sidebar"
     :category "Layout"
     :description "A GtkStack holds several pages and shows one at a time; a GtkStackSidebar
lists them. Pages slide when you switch.")
  (let ((window (make-demo-frame "Stack and sidebar" :width 560 :height 340))
        (stack (gtk:stack-new))
        (sidebar (gtk:stack-sidebar-new)))
    (gtk:stack-set-transition-type stack :slide-up-down)
    (loop for (name title text) in '(("welcome" "Welcome" "Pick a page on the left.")
                                     ("lisp" "Lisp" "The page you are looking at is a GtkLabel.")
                                     ("gtk" "GTK" "Stacks are handy for settings panes and wizards.")
                                     ("about" "About" "Built with gtk4 for SBCL."))
          do (let ((label (gtk:label-new text)))
               (gtk:widget-set-hexpand label t)
               (gtk:stack-add-titled stack label name title)))
    (gtk:stack-sidebar-set-stack sidebar stack)
    (gtk:window-set-child window (hbox 0 sidebar (gtk:separator-new :vertical) stack))
    window))
