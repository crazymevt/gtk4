;;;; column-view.lisp — files in columns
;;;; GTK docs: https://docs.gtk.org/gtk4/class.ColumnView.html

(in-package #:gtk4-demo)

(defun file-info-column (title text-of &key expand)
  "A column showing (TEXT-OF file-info) for each row."
  (let ((column (gtk:column-view-column-new title (label-list-factory text-of))))
    (gtk:column-view-column-set-expand column expand)
    column))

(defun human-size (bytes)
  (cond ((< bytes 1024) (format nil "~d B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024)))
        (t (format nil "~,1f MB" (/ bytes 1024 1024)))))

(define-demo column-view
    (:title "Files in columns"
     :category "Lists"
     :description "A GtkColumnView over a GtkDirectoryList of your home directory. The
directory list loads asynchronously; rows appear as GIO enumerates the files.")
  (let* ((window (make-demo-frame "Files in columns" :width 640 :height 480))
         (files (gtk:directory-list-new "standard::display-name,standard::size,standard::content-type"
                                        (gio:file-new-for-path (namestring (user-homedir-pathname)))))
         (view (gtk:column-view-new (gtk:single-selection-new files))))
    (gtk:column-view-append-column view (file-info-column "Name" #'gio:file-info-get-display-name
                                                          :expand t))
    (gtk:column-view-append-column view (file-info-column "Size"
                                                          (lambda (info) (human-size (gio:file-info-get-size info)))))
    (gtk:column-view-append-column view (file-info-column "Type"
                                                          (lambda (info) (or (gio:file-info-get-content-type info) ""))))
    (gtk:window-set-child window (scrolled view))
    window))
