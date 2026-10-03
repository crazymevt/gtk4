;;;; browser.lisp — the demo browser: pick a demo, read its source, run it

(in-package #:gtk4-demo)

(defun browser-window (app)
  (let* ((demos (coerce (all-demos) 'vector))
         (window (gtk:application-window-new app))
         (titles (gtk:string-list-new
                  (map 'list (lambda (d) (format nil "~a — ~a" (demo-category d) (demo-title d)))
                       demos)))
         (selection (gtk:single-selection-new titles))
         (list (gtk:list-view-new selection
                                  (label-list-factory #'gtk:string-object-get-string)))
         (title (gtk:label-new ""))
         (description (gtk:label-new ""))
         (run (gtk:button-new-with-label "Run demo"))
         (source-view (gtk:text-view-new))
         (stack (gtk:stack-new))
         (switcher (gtk:stack-switcher-new))
         (header (gtk:header-bar-new))
         (paned (gtk:paned-new :horizontal)))
    (labels ((current () (aref demos (gtk:single-selection-get-selected selection)))
             (show-current ()
               (let ((demo (current)))
                 (gtk:label-set-markup title (format nil "<big><b>~a</b></big>"
                                                     (glib:markup-escape-text (demo-title demo) -1)))
                 (gtk:label-set-text description (demo-description demo))
                 (gtk:text-buffer-set-text (gtk:text-view-get-buffer source-view)
                                           (demo-source demo) -1)))
             (run-current ()
               (let ((demo-window (make-demo-window (current))))
                 (gtk:window-set-transient-for demo-window window)
                 (gtk:window-present demo-window))))
      ;; Left: the list of demos.
      (gtk:list-view-set-single-click-activate list nil)
      (gobject:connect list :activate (lambda (list position)
                                        (declare (ignore list position))
                                        (run-current)))
      (gobject:connect selection "notify::selected" (lambda (s p) (declare (ignore s p)) (show-current)))
      (gtk:paned-set-start-child paned (scrolled list))
      (gtk:paned-set-position paned 300)
      ;; Right: information and source.
      (gtk:label-set-xalign title 0.0)
      (gtk:label-set-xalign description 0.0)
      (gtk:label-set-wrap description t)
      (gtk:widget-set-halign run :start)
      (gobject:connect run :clicked (lambda (b) (declare (ignore b)) (run-current)))
      (gtk:stack-add-titled stack (margins (vbox 12 title description run) 24) "info" "Info")
      (gtk:text-view-set-monospace source-view t)
      (gtk:text-view-set-editable source-view nil)
      (gtk:text-view-set-left-margin source-view 12)
      (gtk:stack-add-titled stack (scrolled source-view) "source" "Source")
      (gtk:stack-switcher-set-stack switcher stack)
      (gtk:paned-set-end-child paned stack)
      ;; Window
      (gtk:header-bar-set-title-widget header switcher)
      (gtk:window-set-titlebar window header)
      (gtk:window-set-title window "gtk4 demos")
      (gtk:window-set-default-size window 1000 640)
      (gtk:window-set-child window paned)
      (show-current)
      window)))

(defun main (&key quit-after)
  "Open the demo browser. With QUIT-AFTER (seconds), quit automatically."
  (unless (glib:main-thread-p)
    (error "GTK must run on the main thread; start the demos from a script."))
  (let ((app (gtk:application-new "org.lisp.gtk4.Demo" '(:default-flags))))
    (gobject:connect app :activate (lambda (app) (gtk:window-present (browser-window app))))
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (glib:with-gtk-float-traps
      (gio:application-run app nil))))
