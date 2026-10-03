;;;; adwaita.lisp — a libadwaita application
;;;;
;;;; An adaptive GNOME-style window: a toolbar view with a header bar, a
;;;; preferences page of rows, a toast overlay, and a button that switches
;;;; between light and dark styles. Needs the gtk4-adwaita system (and
;;;; libadwaita 1.5 or newer).
;;;;
;;;; libadwaita documentation:
;;;;   https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/class.ToolbarView.html
;;;;   https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/class.PreferencesPage.html
;;;;   https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/class.ToastOverlay.html
;;;;
;;;; Run with:  make example NAME=adwaita   (QUIT_AFTER=3 to close automatically)

(defpackage #:gtk4-examples.adwaita
  (:use #:cl)
  (:export #:main))

(in-package #:gtk4-examples.adwaita)

(defun toggle-style (button)
  (setf (adw:color-scheme) (if (adw:dark-p) :force-light :force-dark))
  (gtk:button-set-label button (if (adw:dark-p) "Light" "Dark")))

(defun activate (app)
  (multiple-value-bind (window ids)
      (gtk:build
        (adw:application-window :application app :title "Settings"
                                :default-width 420 :default-height 520
          (adw:toast-overlay :id :toasts
            (adw:toolbar-view
              (adw:header-bar :child-type "top"
                (gtk:button :label (if (adw:dark-p) "Light" "Dark") :child-type "end"
                            :tooltip-text "Switch light and dark" :on-clicked 'toggle-style))
              (adw:preferences-page
                (adw:preferences-group :title "Account"
                  (adw:entry-row :id :name :title "Name")
                  (adw:action-row :title "Email" :subtitle "user@example.com"))
                (adw:preferences-group :title "Notifications"
                  (adw:switch-row :id :sounds :title "Play sounds" :active t)
                  (adw:spin-row :title "Reminders per day"
                                :adjustment (gtk:adjustment-new 3d0 0d0 24d0 1d0 4d0 0d0)))
                (adw:preferences-group
                  (gtk:button :id :save :label "Save" :halign :center
                              :css-classes '("pill" "suggested-action"))))))))
    (gobject:connect (gethash :save ids) :clicked
                     (lambda (button)
                       (declare (ignore button))
                       (adw:show-toast (gethash :toasts ids)
                                       (format nil "Saved~@[ for ~a~]"
                                               (let ((name (gtk:editable-get-text (gethash :name ids))))
                                                 (and (plusp (length name)) name)))
                                       :button-label "Undo"
                                       :on-button (lambda (toast)
                                                    (declare (ignore toast))
                                                    (adw:show-toast (gethash :toasts ids) "Undone")))))
    (gtk:window-present window)))

(defun main (&key quit-after)
  (adw:run-application "org.lisp.gtk4.Adwaita" 'activate :quit-after quit-after))
