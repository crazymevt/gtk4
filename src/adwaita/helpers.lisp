;;;; helpers.lisp — a few conveniences for libadwaita
;;;;
;;;; Everything libadwaita offers is in the ADW package under its C names
;;;; (adw:application-new for adw_application_new); see the libadwaita
;;;; documentation. These helpers cover what nearly every app does.

(in-package #:gtk4)

(defun adw:run-application (id activate &key (flags '(:default-flags)) quit-after (argv nil))
  "Create an adw:application with ID, call ACTIVATE (a function of the
application, or a symbol naming one) when it activates, and run it; returns
its exit status. AdwApplication initializes libadwaita and loads the app's
style.css resource. QUIT-AFTER (seconds) quits automatically, for scripts
and tests."
  (let ((app (adw:application-new id flags)))
    (gobject:connect app :activate activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (glib:with-gtk-float-traps
      (gio:application-run app argv))))

(defun adw:color-scheme ()
  "The application's color scheme: :default (follow the system),
:force-light, :prefer-light, :prefer-dark or :force-dark. Settable with setf.

See: https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/property.StyleManager.color-scheme.html"
  (adw:style-manager-get-color-scheme (adw:style-manager-get-default)))

(defun (setf adw:color-scheme) (scheme)
  (adw:style-manager-set-color-scheme (adw:style-manager-get-default) scheme)
  scheme)

(defun adw:dark-p ()
  "True when the application is currently using a dark style."
  (adw:style-manager-get-dark (adw:style-manager-get-default)))

(defun adw:show-toast (overlay title &key (timeout 5) button-label on-button (priority :normal))
  "Show a toast with TITLE in OVERLAY (an adw:toast-overlay) for TIMEOUT
seconds (0 keeps it until dismissed). With BUTTON-LABEL, the toast has a
button that calls ON-BUTTON (a function of the toast). Returns the toast."
  (let ((toast (adw:toast-new title)))
    (adw:toast-set-timeout toast timeout)
    (adw:toast-set-priority toast priority)
    (when button-label
      (adw:toast-set-button-label toast button-label)
      (when on-button
        (gobject:connect toast :button-clicked on-button)))
    (adw:toast-overlay-add-toast overlay toast)
    toast))
