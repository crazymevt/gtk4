;;;; editor.lisp — the text editor built step by step in the manual's tutorial
;;;;
;;;; A window class defined in Lisp, a text view with a live word count,
;;;; opening and saving files with GTK's file dialog and GIO's asynchronous
;;;; I/O, and errors shown in an alert. Signal handlers are connected by
;;;; symbol, so redefining one at the REPL changes the running program.
;;;;
;;;; GTK documentation:
;;;;   https://docs.gtk.org/gtk4/class.TextView.html
;;;;   https://docs.gtk.org/gtk4/class.FileDialog.html
;;;;   https://docs.gtk.org/gio/method.File.load_contents_async.html
;;;;
;;;; Run with:  make example NAME=editor   (QUIT_AFTER=3 to close automatically)

(defpackage #:gtk4-examples.editor
  (:use #:cl)
  (:export #:main #:editor-window))

(in-package #:gtk4-examples.editor)

;;; The window

(defclass editor-window (gtk:application-window)
  ((buffer :reader editor-buffer)
   (status :reader editor-status)
   (file :initform nil :accessor editor-file))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispEditorWindow"))

;;; The word count

(defun buffer-text (buffer)
  (multiple-value-bind (start end) (gtk:text-buffer-get-bounds buffer)
    (gtk:text-buffer-get-text buffer start end nil)))

(defun word-count (text)
  (loop with in-word = nil
        for c across text
        count (and (not (member c '(#\Space #\Tab #\Newline #\Return)))
                   (not in-word))
          into words
        do (setf in-word (not (member c '(#\Space #\Tab #\Newline #\Return))))
        finally (return words)))

(defun show-status (window)
  (let ((text (buffer-text (editor-buffer window))))
    (gtk:label-set-text (editor-status window)
                        (format nil "~:d word~:p, ~:d character~:p~@[ — ~a~]"
                                (word-count text) (length text)
                                (and (editor-file window)
                                     (gio:file-get-basename (editor-file window)))))))

;;; Building the window

(defmethod initialize-instance :after ((window editor-window) &key)
  (multiple-value-bind (content ids)
      (gtk:build
        (gtk:box :orientation :vertical
          (gtk:scrolled-window :vexpand t
            (gtk:text-view :id :text :monospace t :wrap-mode :word-char
                           :top-margin 12 :bottom-margin 12 :left-margin 12 :right-margin 12))
          (gtk:label :id :status :xalign 0.0 :margin-start 12 :margin-end 12
                     :margin-top 6 :margin-bottom 6 :css-classes '("dim-label"))))
    (setf (slot-value window 'buffer) (gtk:text-view-get-buffer (gethash :text ids))
          (slot-value window 'status) (gethash :status ids))
    (gtk:window-set-titlebar
     window (gtk:build
              (gtk:header-bar
                (gtk:button :label "Open" :child-type "start" :on-clicked 'open-clicked)
                (gtk:button :label "Save" :child-type "end" :on-clicked 'save-clicked
                            :css-classes '("suggested-action")))))
    (gtk:window-set-child window content)
    (gtk:window-set-default-size window 600 420)
    ;; The closure calls SHOW-STATUS by name, so redefining it takes effect.
    (gobject:connect (editor-buffer window) :changed
                     (lambda (buffer) (declare (ignore buffer)) (show-status window)))
    (show-status window)))

(defun editor-of (widget)
  "The editor window WIDGET is in."
  (gtk:widget-get-root widget))

;;; Opening and saving

(defun report-error (window condition)
  "Show CONDITION in an alert, unless the user just closed a dialog."
  (unless (typep condition 'gtk:dialog-error-dismissed)
    ;; gtk_alert_dialog_new takes printf-style arguments, so it has no
    ;; binding; make-instance sets the same properties.
    (gtk:alert-dialog-show (make-instance 'gtk:alert-dialog
                                          :message "Something went wrong"
                                          :detail (glib:glib-error-message condition))
                           window)))

(defun load-file (window file)
  (gio:async (gio:file-load-contents-async file)
             (lambda (ok contents etag)
               (declare (ignore ok etag))
               (setf (editor-file window) file)
               (gtk:text-buffer-set-text (editor-buffer window)
                                         (sb-ext:octets-to-string
                                          (coerce contents '(vector (unsigned-byte 8)))
                                          :external-format :utf-8)
                                         -1))
             :error (lambda (e) (report-error window e))))

(defun open-clicked (button)
  (let ((window (editor-of button)))
    (gio:async (gtk:file-dialog-open (gtk:file-dialog-new) window)
               (lambda (file) (load-file window file))
               :error (lambda (e) (report-error window e)))))

(defun save-file (window file)
  (let ((octets (sb-ext:string-to-octets (buffer-text (editor-buffer window))
                                         :external-format :utf-8)))
    (gio:async (gio:file-replace-contents-async file octets nil nil '(:none))
               (lambda (ok etag)
                 (declare (ignore ok etag))
                 (setf (editor-file window) file)
                 (show-status window))
               :error (lambda (e) (report-error window e)))))

(defun save-clicked (button)
  (let ((window (editor-of button)))
    (if (editor-file window)
        (save-file window (editor-file window))
        (gio:async (gtk:file-dialog-save (gtk:file-dialog-new) window)
                   (lambda (file) (save-file window file))
                   :error (lambda (e) (report-error window e))))))

;;; The application

(defun activate (app)
  (gtk:window-present (make-instance 'editor-window :application app :title "Editor")))

(defun main (&key quit-after)
  (let ((app (gtk:application-new "org.lisp.gtk4.Editor" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (gio:application-run app nil)))
