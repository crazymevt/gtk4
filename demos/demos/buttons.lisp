;;;; buttons.lisp — the button family
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Button.html

(in-package #:gtk4-demo)

(define-demo buttons
    (:title "Buttons"
     :category "Basics"
     :description "Push buttons, toggle buttons, check buttons, radio groups and a switch. The
label at the bottom reports each change.")
  (let* ((window (make-demo-frame "Buttons" :width 360 :height 320))
         (status (gtk:label-new "Use a control"))
         (push (gtk:button-new-with-label "Push button"))
         (toggle (gtk:toggle-button-new-with-label "Toggle button"))
         (check (gtk:check-button-new-with-label "Check button"))
         (small (gtk:check-button-new-with-label "Small"))
         (large (gtk:check-button-new-with-label "Large"))
         (switch (gtk:switch-new)))
    (flet ((say (fmt &rest args) (gtk:label-set-text status (apply #'format nil fmt args))))
      (gobject:connect push :clicked (lambda (b) (declare (ignore b)) (say "Pushed")))
      (gobject:connect toggle :toggled
                       (lambda (b) (say "Toggle is ~:[up~;down~]" (gtk:toggle-button-get-active b))))
      (gobject:connect check :toggled
                       (lambda (b) (say "Check is ~:[off~;on~]" (gtk:check-button-get-active b))))
      ;; Check buttons in a group behave as radio buttons.
      (gtk:check-button-set-group large small)
      (gtk:check-button-set-active small t)
      (dolist (b (list small large))
        (gobject:connect b :toggled
                         (lambda (b) (when (gtk:check-button-get-active b)
                                       (say "Size: ~a" (gtk:check-button-get-label b))))))
      (gobject:connect switch "notify::active"
                       (lambda (s p) (declare (ignore p)) (say "Switch is ~:[off~;on~]" (gtk:switch-get-active s)))))
    (gtk:widget-set-halign switch :start)
    (gtk:window-set-child window
                          (margins (vbox 12 push toggle check (hbox 12 small large) switch status) 24))
    window))
