;;;; key-events.lisp — reading the keyboard
;;;; GTK docs: https://docs.gtk.org/gtk4/class.EventControllerKey.html

(in-package #:gtk4-demo)

(define-demo key-events
    (:title "Key events"
     :category "Input"
     :description "A GtkEventControllerKey on the window reports each key press with its key
name and modifier state.")
  (let* ((window (make-demo-frame "Key events" :width 380 :height 180))
         (label (gtk:label-new "Press any key"))
         (keys (gtk:event-controller-key-new)))
    (gtk:widget-add-css-class label "title-3")
    (gobject:connect keys :key-pressed
                     (lambda (controller keyval keycode state)
                       (declare (ignore controller keycode))
                       (gtk:label-set-text label (format nil "~{~a+~}~a"
                                                         (mapcar #'string-capitalize
                                                                 (remove :lock-mask
                                                                         (if (listp state) state (list state))))
                                                         (or (gdk:keyval-name keyval) "?")))
                       nil))           ; nil lets other handlers see the key too
    (gtk:widget-add-controller window keys)
    (gtk:window-set-child window label)
    window))
