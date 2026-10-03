;;;; paint.lisp — drawing with the pointer
;;;; GTK docs: https://docs.gtk.org/gtk4/class.GestureDrag.html

(in-package #:gtk4-demo)

(define-demo paint
    (:title "Paint"
     :category "Drawing"
     :description "Drag in the window to paint. A GtkGestureDrag reports the pointer; strokes
are kept as lists of points and replayed by the cairo draw function. Right-click clears.")
  (let* ((window (make-demo-frame "Paint" :width 520 :height 400))
         (area (gtk:drawing-area-new))
         (drag (gtk:gesture-drag-new))
         (clear (gtk:gesture-click-new))
         (strokes '())                  ; each stroke: list of (x . y), newest first
         (origin nil))
    (gtk:drawing-area-set-draw-func
     area (lambda (area cr width height)
            (declare (ignore area))
            (cairo:set-source-rgb cr 1 1 1)
            (cairo:rectangle cr 0 0 width height)
            (cairo:fill cr)
            (cairo:set-source-rgb cr 0.15 0.3 0.7)
            (cairo:set-line-width cr 4)
            (cairo:set-line-cap cr :round)
            (cairo:set-line-join cr :round)
            (dolist (stroke strokes)
              (let ((points (reverse stroke)))
                (cairo:move-to cr (car (first points)) (cdr (first points)))
                (dolist (p (rest points)) (cairo:line-to cr (car p) (cdr p)))
                (cairo:stroke cr)))))
    (gobject:connect drag :drag-begin
                     (lambda (gesture x y)
                       (declare (ignore gesture))
                       (setf origin (cons x y))
                       (push (list (cons x y)) strokes)))
    (gobject:connect drag :drag-update
                     (lambda (gesture dx dy)
                       (declare (ignore gesture))
                       (push (cons (+ (car origin) dx) (+ (cdr origin) dy)) (first strokes))
                       (gtk:widget-queue-draw area)))
    (gtk:gesture-single-set-button clear 3)
    (gobject:connect clear :pressed
                     (lambda (gesture n x y)
                       (declare (ignore gesture n x y))
                       (setf strokes '())
                       (gtk:widget-queue-draw area)))
    (gtk:widget-add-controller area drag)
    (gtk:widget-add-controller area clear)
    (gtk:window-set-child window area)
    window))
