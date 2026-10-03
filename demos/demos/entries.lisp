;;;; entries.lisp — text and number input
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Entry.html

(in-package #:gtk4-demo)

(define-demo entries
    (:title "Entries and numbers"
     :category "Input"
     :description "A text entry, a password entry, a spin button and a scale. The summary
line updates as you edit any of them.")
  (let* ((window (make-demo-frame "Entries and numbers" :width 420 :height 300))
         (name (gtk:entry-new))
         (password (gtk:password-entry-new))
         (age (gtk:spin-button-new-with-range 0 120 1))
         (volume (gtk:scale-new-with-range :horizontal 0 100 1))
         (summary (gtk:label-new "")))
    (gtk:entry-set-placeholder-text name "Your name")
    (gtk:password-entry-set-show-peek-icon password t)
    (gtk:spin-button-set-value age 30)
    (gtk:range-set-value volume 50)
    (gtk:scale-set-draw-value volume t)
    (flet ((update (&rest args)
             (declare (ignore args))
             (gtk:label-set-text summary
                                 (format nil "~a, age ~d, volume ~d, password of ~d characters"
                                         (let ((n (gtk:editable-get-text name)))
                                           (if (string= n "") "Someone" n))
                                         (gtk:spin-button-get-value-as-int age)
                                         (round (gtk:range-get-value volume))
                                         (length (gtk:editable-get-text password))))))
      (gobject:connect name :changed #'update)
      (gobject:connect password :changed #'update)
      (gobject:connect age :value-changed #'update)
      (gobject:connect volume :value-changed #'update)
      (update))
    (gtk:window-set-child window (margins (vbox 12 name password age volume summary) 24))
    window))
