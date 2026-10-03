;;;; gtk.lisp — GTK through the generated bindings
;;;;
;;;; GTK needs a display; without one these tests are skipped.

(in-package #:gtk4-tests)

(define-test gtk :parent gtk4-tests)

(defvar *gtk-available* :unknown)

(defun gtk-available-p ()
  (when (eq *gtk-available* :unknown)
    (setf *gtk-available* (and (rt:main-thread-p) (ignore-errors (gtk:init-check)))))
  *gtk-available*)

(defmacro with-gtk (&body body)
  `(if (gtk-available-p)
       (progn ,@body)
       (skip "no display, or not on the main thread" (true t))))

(define-test widgets-and-properties :parent gtk
  (with-gtk
    (let ((button (make-instance 'gtk:button :label "Save" :halign :center)))
      (is string= "Save" (gtk:button-get-label button))
      (is eq :center (gtk:widget-halign button))
      (setf (gtk:button-label button) "Saved")
      (is string= "Saved" (gtk:button-label button))
      (true (typep button 'gtk:widget))
      (true (typep button 'gtk:actionable)))))

(define-test widget-trees :parent gtk
  (with-gtk
    (let ((box (gtk:box-new :vertical 6)))
      (dolist (text '("one" "two" "three"))
        (gtk:box-append box (gtk:label-new text)))
      (is equal '("one" "two" "three")
          (loop for child = (gtk:widget-get-first-child box) then (gtk:widget-get-next-sibling child)
                while child
                collect (gtk:label-get-text child)))
      (is eq box (gtk:widget-get-parent (gtk:widget-get-first-child box))))))

(define-test signals-on-widgets :parent gtk
  (with-gtk
    (let* ((button (gtk:button-new-with-label "Go"))
           (clicks 0))
      (rt:connect button :clicked (lambda (b) (declare (ignore b)) (incf clicks)))
      (rt:emit button :clicked)
      (rt:emit button :clicked)
      (is = 2 clicks))
    (let* ((check (gtk:check-button-new-with-label "Agree"))
           (seen '()))
      (rt:connect check :toggled (lambda (c) (push (gtk:check-button-get-active c) seen)))
      (setf (gtk:check-button-active check) t)
      (setf (gtk:check-button-active check) nil)
      (is equal '(nil t) seen))))

(define-test list-models :parent gtk
  (with-gtk
    (let ((strings (gtk:string-list-new '("red" "green" "blue"))))
      (is = 3 (gio:list-model-get-n-items strings))
      (is string= "green" (gtk:string-list-get-string strings 1))
      (gtk:string-list-append strings "violet")
      (is = 4 (gio:list-model-get-n-items strings)))))

(define-test windows :parent gtk
  (with-gtk
    (let ((window (make-instance 'gtk:window :title "Test" :default-width 200)))
      (gtk:window-set-child window (gtk:label-new "inside"))
      (is string= "Test" (gtk:window-get-title window))
      (is string= "inside" (gtk:label-get-text (gtk:window-get-child window)))
      (gtk:window-destroy window))))
