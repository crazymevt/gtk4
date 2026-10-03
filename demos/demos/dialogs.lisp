;;;; dialogs.lisp — asking the user something
;;;; GTK docs: https://docs.gtk.org/gtk4/class.AlertDialog.html, https://docs.gtk.org/gtk4/class.FileDialog.html

(in-package #:gtk4-demo)

(define-demo dialogs
    (:title "Dialogs"
     :category "Dialogs"
     :description "GtkAlertDialog and GtkFileDialog are asynchronous: the Lisp callback runs
when the user answers, and the matching -finish function returns the answer (or signals an
error if the dialog was dismissed).")
  (let* ((window (make-demo-frame "Dialogs" :width 380 :height 220))
         (status (gtk:label-new "Open a dialog"))
         (ask (gtk:button-new-with-label "Ask a question"))
         (open (gtk:button-new-with-label "Choose a file")))
    (gobject:connect ask :clicked
                     (lambda (b)
                       (declare (ignore b))
                       (let ((dialog (make-instance 'gtk:alert-dialog
                                                    :message "Save changes?"
                                                    :detail "Unsaved changes will be lost."
                                                    :buttons '("Cancel" "Discard" "Save")
                                                    :cancel-button 0
                                                    :default-button 2)))
                         (gtk:alert-dialog-choose
                          dialog window nil
                          (lambda (dialog result)
                            (let ((choice (handler-case (gtk:alert-dialog-choose-finish dialog result)
                                            (glib:glib-error () 0))))
                              (gtk:label-set-text status
                                                  (format nil "You chose ~a"
                                                          (nth choice '("Cancel" "Discard" "Save"))))))))))
    (gobject:connect open :clicked
                     (lambda (b)
                       (declare (ignore b))
                       (gtk:file-dialog-open
                        (gtk:file-dialog-new) window nil
                        (lambda (dialog result)
                          (handler-case
                              (gtk:label-set-text status
                                                  (format nil "Chose ~a"
                                                          (gio:file-get-basename
                                                           (gtk:file-dialog-open-finish dialog result))))
                            (glib:glib-error () (gtk:label-set-text status "No file chosen")))))))
    (gtk:window-set-child window (margins (vbox 12 ask open status) 24))
    window))
