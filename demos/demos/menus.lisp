;;;; menus.lisp — menus backed by actions
;;;; GTK docs: https://docs.gtk.org/gio/class.Menu.html, https://docs.gtk.org/gtk4/class.PopoverMenu.html

(in-package #:gtk4-demo)

(define-demo menus
    (:title "Menus and actions"
     :category "Actions"
     :description "A GMenu model shown by a GtkMenuButton. Each item names an action in a
GSimpleActionGroup inserted into the window as \"demo\"; one action is stateful and shows a
check mark. Ctrl+N triggers the New action through a shortcut controller.")
  (let* ((window (make-demo-frame "Menus and actions" :width 380 :height 240))
         (status (gtk:label-new "Choose a menu item"))
         (group (gio:simple-action-group-new))
         (menu (gio:menu-new))
         (button (gtk:menu-button-new))
         (header (gtk:header-bar-new)))
    (flet ((add-action (name function)
             (let ((action (gio:simple-action-new name nil)))
               (gobject:connect action :activate
                                (lambda (action parameter)
                                  (declare (ignore action parameter))
                                  (funcall function)))
               (gio:action-map-add-action group action))))
      (add-action "new" (lambda () (gtk:label-set-text status "New document")))
      (add-action "open" (lambda () (gtk:label-set-text status "Open…")))
      (add-action "quit" (lambda () (gtk:window-close window))))
    ;; A stateful boolean action: its state is a GVariant.
    (let ((wrap (gio:simple-action-new-stateful "wrap" nil (glib:variant-new-boolean nil))))
      (gobject:connect wrap :change-state
                       (lambda (action value)
                         (gio:simple-action-set-state action value)
                         (gtk:label-set-text status (format nil "Wrap lines: ~:[off~;on~]"
                                                            (glib:variant-get-boolean value)))))
      (gio:action-map-add-action group wrap))
    (gio:menu-append menu "New" "demo.new")
    (gio:menu-append menu "Open…" "demo.open")
    (gio:menu-append menu "Wrap lines" "demo.wrap")
    (gio:menu-append menu "Close window" "demo.quit")
    (gtk:widget-insert-action-group window "demo" group)
    (gtk:menu-button-set-menu-model button menu)
    (gtk:menu-button-set-icon-name button "open-menu-symbolic")
    (gtk:header-bar-pack-end header button)
    (gtk:window-set-titlebar window header)
    ;; Keyboard shortcut for the New action.
    (let ((shortcuts (gtk:shortcut-controller-new)))
      (gtk:shortcut-controller-add-shortcut
       shortcuts (gtk:shortcut-new (gtk:shortcut-trigger-parse-string "<Control>n")
                                   (gtk:named-action-new "demo.new")))
      (gtk:widget-add-controller window shortcuts))
    (gtk:window-set-child window status)
    window))
