;;;; text-view.lisp — multi-line text with formatting tags
;;;; GTK docs: https://docs.gtk.org/gtk4/section-text-widget.html

(in-package #:gtk4-demo)

(define-demo text-view
    (:title "Text view and tags"
     :category "Text"
     :description "A GtkTextView whose buffer uses tags for bold, italic, colored and large
text. The buttons apply a tag to the current selection.")
  (let* ((window (make-demo-frame "Text view and tags" :width 520 :height 380))
         (view (gtk:text-view-new))
         (buffer (gtk:text-view-get-buffer view))
         (table (gtk:text-buffer-get-tag-table buffer)))
    ;; Tags are objects with properties; add them to the buffer's tag table.
    (dolist (spec '(("bold" :weight 700)
                    ("italic" :style :italic)
                    ("red" :foreground "firebrick")
                    ("large" :scale 1.6d0)))
      (gtk:text-tag-table-add table (apply #'make-instance 'gtk:text-tag :name spec)))
    (gtk:text-view-set-wrap-mode view :word)
    (gtk:text-view-set-left-margin view 12)
    (gtk:text-view-set-top-margin view 12)
    (flet ((insert (text &rest tags)
             (let ((start-offset (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter buffer))))
               (gtk:text-buffer-insert buffer (gtk:text-buffer-get-end-iter buffer) text -1)
               (dolist (tag tags)
                 (gtk:text-buffer-apply-tag-by-name buffer tag
                                                    (gtk:text-buffer-get-iter-at-offset buffer start-offset)
                                                    (gtk:text-buffer-get-end-iter buffer))))))
      (insert "Formatted text" "large" "bold")
      (insert (format nil "~%~%Text can be "))
      (insert "bold" "bold")
      (insert ", ")
      (insert "italic" "italic")
      (insert ", or ")
      (insert "colored" "red")
      (insert (format nil ". Select some text and use the buttons below.~%")))
    (let ((buttons (hbox 6)))
      (dolist (tag '("bold" "italic" "red"))
        (let ((b (gtk:button-new-with-label tag)))
          (gobject:connect b :clicked
                           (lambda (b)
                             (multiple-value-bind (has-selection start end)
                                 (gtk:text-buffer-get-selection-bounds buffer)
                               (when has-selection
                                 (gtk:text-buffer-apply-tag-by-name buffer (gtk:button-get-label b)
                                                                    start end)))))
          (gtk:box-append buttons b)))
      (gtk:window-set-child window (vbox 6 (scrolled view) (margins buttons 6))))
    window))
