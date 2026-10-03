;;;; drawing.lisp — custom drawing with GtkDrawingArea and cairo
;;;;
;;;; A window whose contents are drawn with cairo: a gradient background, a
;;;; circle that follows the window size, and centered text. Resize the
;;;; window to see it redraw.
;;;;
;;;; GTK documentation: https://docs.gtk.org/gtk4/class.DrawingArea.html
;;;; cairo manual:      https://www.cairographics.org/manual/
;;;;
;;;; Run with:  make example NAME=drawing   (QUIT_AFTER=3 to close automatically)

(defpackage #:gtk4-examples.drawing
  (:use #:cl)
  (:export #:main))

(in-package #:gtk4-examples.drawing)

(defun draw (area cr width height)
  "The draw function: GTK calls it with a cairo context sized WIDTH by HEIGHT."
  (declare (ignore area))
  ;; Background: a vertical gradient.
  (let ((gradient (cairo:pattern-create-linear 0 0 0 height)))
    (cairo:pattern-add-color-stop-rgb gradient 0 0.20 0.40 0.75)
    (cairo:pattern-add-color-stop-rgb gradient 1 0.05 0.10 0.25)
    (cairo:set-source cr gradient)
    (cairo:paint cr))
  ;; A circle, as large as the window allows.
  (let ((radius (* 0.35 (min width height))))
    (cairo:arc cr (/ width 2) (/ height 2) radius 0 (* 2 pi))
    (cairo:set-source-rgba cr 1 0.8 0.2 0.9)
    (cairo:fill-preserve cr)
    (cairo:set-source-rgb cr 1 1 1)
    (cairo:set-line-width cr 3)
    (cairo:stroke cr))
  ;; Text, centered using its extents.
  (cairo:select-font-face cr "Sans" :normal :bold)
  (cairo:set-font-size cr 24)
  (let* ((text "Drawn with cairo")
         (extents (cairo:text-extents cr text)))
    (cairo:move-to cr
                   (- (/ width 2) (/ (cairo:text-extents-width extents) 2)
                      (cairo:text-extents-x-bearing extents))
                   (- (/ height 2) (/ (cairo:text-extents-height extents) 2)
                      (cairo:text-extents-y-bearing extents)))
    (cairo:set-source-rgb cr 0.1 0.1 0.1)
    (cairo:show-text cr text)))

(defun activate (app)
  (let ((window (gtk:application-window-new app))
        (area (gtk:drawing-area-new)))
    (gtk:window-set-title window "Drawing with cairo")
    (gtk:window-set-default-size window 480 320)
    ;; Passing the symbol, not #'draw, means redefining DRAW at the REPL
    ;; changes what the running window paints on its next redraw.
    (gtk:drawing-area-set-draw-func area 'draw)
    (gtk:window-set-child window area)
    (gtk:window-present window)))

(defun main (&key quit-after)
  "Open the drawing window. With QUIT-AFTER (seconds), quit automatically."
  (unless (glib:main-thread-p)
    (error "GTK must run on the main thread; start this from a script."))
  (let ((app (gtk:application-new "org.lisp.gtk4.Drawing" '(:default-flags))))
    (gobject:connect app :activate #'activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (glib:with-gtk-float-traps
      (gio:application-run app nil))))
