;;;; progress.lisp — showing that work is happening
;;;; GTK docs: https://docs.gtk.org/gtk4/class.ProgressBar.html

(in-package #:gtk4-demo)

(define-demo progress
    (:title "Progress and spinners"
     :category "Feedback"
     :description "A determinate progress bar advanced by a GLib timeout, a pulsing one for
work of unknown length, and a spinner. The timeout stops itself when the window closes.")
  (let* ((window (make-demo-frame "Progress and spinners" :width 380 :height 220))
         (bar (gtk:progress-bar-new))
         (pulse (gtk:progress-bar-new))
         (spinner (gtk:spinner-new))
         (running t))
    (gtk:progress-bar-set-show-text bar t)
    (gtk:spinner-start spinner)
    (glib:timeout-add glib:+priority-default+ 100
                      (lambda ()
                        (when running
                          (let ((f (+ (gtk:progress-bar-get-fraction bar) 0.02)))
                            (gtk:progress-bar-set-fraction bar (if (> f 1) 0d0 f)))
                          (gtk:progress-bar-pulse pulse))
                        running))      ; returning false removes the timeout
    (gobject:connect window :close-request (lambda (w) (declare (ignore w)) (setf running nil) nil))
    (gtk:window-set-child window (margins (vbox 18 bar pulse spinner) 24))
    window))
