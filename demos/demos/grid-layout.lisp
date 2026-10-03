;;;; grid-layout.lisp — a keypad laid out with GtkGrid
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Grid.html

(in-package #:gtk4-demo)

(define-demo grid-layout
    (:title "Grid layout"
     :category "Layout"
     :description "A number pad built with a grid. The display spans three columns and the
zero key spans two; the C key clears the display.")
  (let ((window (make-demo-frame "Grid layout" :width 280 :height 360))
        (grid (gtk:grid-new))
        (display (gtk:label-new "")))
    (gtk:grid-set-row-spacing grid 6)
    (gtk:grid-set-column-spacing grid 6)
    (gtk:grid-set-row-homogeneous grid t)
    (gtk:grid-set-column-homogeneous grid t)
    (gtk:label-set-xalign display 1.0)
    (gtk:grid-attach grid display 0 0 3 1)
    (flet ((key (text column row &optional (width 1))
             (let ((b (gtk:button-new-with-label text)))
               (gobject:connect b :clicked
                                (lambda (b)
                                  (let ((pressed (gtk:button-get-label b)))
                                    (gtk:label-set-text display
                                                        (if (string= pressed "C")
                                                            ""
                                                            (concatenate 'string (gtk:label-get-text display)
                                                                         pressed))))))
               (gtk:grid-attach grid b column row width 1))))
      (loop for digit from 1 to 9
            do (key (princ-to-string digit) (mod (1- digit) 3) (- 3 (floor (1- digit) 3))))
      (key "0" 0 4 2)
      (key "C" 2 4))
    (gtk:window-set-child window (margins grid 18))
    window))
