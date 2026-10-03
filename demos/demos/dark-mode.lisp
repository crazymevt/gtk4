;;;; dark-mode.lisp — preferring the dark theme variant
;;;; GTK docs: https://docs.gtk.org/gtk4/property.Settings.gtk-application-prefer-dark-theme.html

(in-package #:gtk4-demo)

(define-demo dark-mode
    (:title "Dark mode"
     :category "Theming"
     :description "Toggles GtkSettings:gtk-application-prefer-dark-theme, which switches every
window of the application to the theme's dark variant.")
  (let* ((window (make-demo-frame "Dark mode" :width 320 :height 160))
         (settings (gtk:settings-get-default))
         (switch (gtk:switch-new))
         (label (gtk:label-new "Prefer dark theme")))
    (gtk:switch-set-active switch (gobject:property settings :gtk-application-prefer-dark-theme))
    (gobject:connect switch "notify::active"
                     (lambda (s p)
                       (declare (ignore p))
                       (setf (gobject:property settings :gtk-application-prefer-dark-theme)
                             (gtk:switch-get-active s))))
    (let ((row (hbox 12 label switch)))
      (gtk:widget-set-halign row :center)
      (gtk:widget-set-valign row :center)
      (gtk:window-set-child window row))
    window))
