;;;; flow-box.lisp — a reflowing grid of color swatches
;;;; GTK docs: https://docs.gtk.org/gtk4/class.FlowBox.html

(in-package #:gtk4-demo)

(defun color-swatch (name)
  "A small drawing area filled with the named color, labelled underneath."
  (let ((color (gdk:make-rgba))
        (area (gtk:drawing-area-new)))
    (gdk:rgba-parse color name)
    (gtk:drawing-area-set-content-width area 64)
    (gtk:drawing-area-set-content-height area 40)
    (gtk:drawing-area-set-draw-func area
                                    (lambda (area cr width height)
                                      (declare (ignore area))
                                      (cairo:set-source-rgb cr (gdk:rgba-red color) (gdk:rgba-green color)
                                                            (gdk:rgba-blue color))
                                      (cairo:rectangle cr 0 0 width height)
                                      (cairo:fill cr)))
    (vbox 4 area (gtk:label-new name))))

(define-demo flow-box
    (:title "Flow box"
     :category "Lists"
     :description "A GtkFlowBox lays its children out in rows and reflows them when the window
is resized. Each swatch is a drawing area painted with cairo.")
  (let ((window (make-demo-frame "Flow box" :width 520 :height 420))
        (flow (gtk:flow-box-new)))
    (gtk:flow-box-set-selection-mode flow :none)
    (gtk:flow-box-set-max-children-per-line flow 12)
    (dolist (name *color-names*)
      (gtk:flow-box-append flow (color-swatch name)))
    (gtk:window-set-child window (scrolled (margins flow 12)))
    window))
