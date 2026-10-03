;;;; drawing.lisp — custom drawing with cairo
;;;; GTK docs: https://docs.gtk.org/gtk4/class.DrawingArea.html

(in-package #:gtk4-demo)

(defun draw-scene (area cr width height)
  (declare (ignore area))
  (let ((gradient (cairo:pattern-create-linear 0 0 0 height)))
    (cairo:pattern-add-color-stop-rgb gradient 0 0.20 0.40 0.75)
    (cairo:pattern-add-color-stop-rgb gradient 1 0.05 0.10 0.25)
    (cairo:set-source cr gradient)
    (cairo:paint cr))
  (let ((r (* 0.3 (min width height))))
    (cairo:arc cr (/ width 2) (/ height 2) r 0 (* 2 pi))
    (cairo:set-source-rgba cr 1 0.8 0.2 0.9)
    (cairo:fill-preserve cr)
    (cairo:set-source-rgb cr 1 1 1)
    (cairo:set-line-width cr 3)
    (cairo:stroke cr))
  (cairo:select-font-face cr "Sans" :normal :bold)
  (cairo:set-font-size cr 22)
  (let* ((text "Drawn with cairo")
         (e (cairo:text-extents cr text)))
    (cairo:move-to cr (- (/ width 2) (/ (cairo:text-extents-width e) 2) (cairo:text-extents-x-bearing e))
                   (- height 24))
    (cairo:set-source-rgb cr 1 1 1)
    (cairo:show-text cr text)))

(define-demo drawing
    (:title "Drawing area"
     :category "Drawing"
     :description "A GtkDrawingArea painted by a Lisp function with cairo: a gradient, a circle
sized to the window, and text centered with its extents. The draw function is passed as a
symbol, so redefining DRAW-SCENE at the REPL changes the drawing.")
  (let ((window (make-demo-frame "Drawing area"))
        (area (gtk:drawing-area-new)))
    (gtk:drawing-area-set-draw-func area 'draw-scene)
    (gtk:window-set-child window area)
    window))
