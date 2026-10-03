;;;; clipboard.lisp — copy and paste
;;;; GTK docs: https://docs.gtk.org/gdk4/class.Clipboard.html

(in-package #:gtk4-demo)

(define-demo clipboard
    (:title "Clipboard"
     :category "Data exchange"
     :description "Copies the entry's text to the clipboard, and pastes it back
asynchronously: GDK reads the clipboard in the background and calls the Lisp function with
the result.")
  (let* ((window (make-demo-frame "Clipboard" :width 400 :height 200))
         (source (gtk:entry-new))
         (copy (gtk:button-new-with-label "Copy"))
         (paste (gtk:button-new-with-label "Paste"))
         (pasted (gtk:label-new "Nothing pasted yet")))
    (gtk:editable-set-text source "Text to copy")
    (gobject:connect copy :clicked
                     (lambda (b)
                       (declare (ignore b))
                       (gdk:clipboard-set-text (gtk:widget-get-clipboard window)
                                               (gtk:editable-get-text source))))
    (gobject:connect paste :clicked
                     (lambda (b)
                       (declare (ignore b))
                       (gdk:clipboard-read-text-async
                        (gtk:widget-get-clipboard window) nil
                        (lambda (clipboard result)
                          (gtk:label-set-text pasted
                                              (format nil "Pasted: ~a"
                                                      (or (gdk:clipboard-read-text-finish clipboard result)
                                                          "(no text)")))))))
    (gtk:window-set-child window (margins (vbox 12 (hbox 6 source copy paste) pasted) 24))
    window))
