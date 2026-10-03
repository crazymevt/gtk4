;;;; css-basics.lisp — styling widgets with CSS
;;;; GTK docs: https://docs.gtk.org/gtk4/css-overview.html

(in-package #:gtk4-demo)

(defparameter *demo-css* "
.demo-card { background: rgba(53, 132, 228, 0.12); border-radius: 12px; padding: 18px; }
.demo-card label.heading { font-size: 18pt; font-weight: bold; }
button.pill { border-radius: 999px; padding: 6px 18px; }
button.danger { background: #c01c28; color: white; }
")

(define-demo css-basics
    (:title "CSS basics"
     :category "Theming"
     :description "A GtkCssProvider loads a small style sheet. CSS classes added with
gtk:widget-add-css-class select which rules apply. The provider is attached to this window's
display.")
  (let* ((window (make-demo-frame "CSS basics" :width 420 :height 300))
         (provider (gtk:css-provider-new))
         (heading (gtk:label-new "Styled with CSS"))
         (body (gtk:label-new "Rounded corners, an accent tint and pill-shaped buttons."))
         (ok (gtk:button-new-with-label "Pill button"))
         (danger (gtk:button-new-with-label "Danger"))
         (card (vbox 12 heading body (hbox 12 ok danger))))
    (gtk:css-provider-load-from-string provider *demo-css*)
    (gtk:style-context-add-provider-for-display (gtk:widget-get-display window) provider
                                                gtk:+style-provider-priority-application+)
    (gtk:widget-add-css-class heading "heading")
    (gtk:label-set-wrap body t)
    (gtk:widget-add-css-class card "demo-card")
    (gtk:widget-add-css-class ok "pill")
    (gtk:widget-add-css-class danger "pill")
    (gtk:widget-add-css-class danger "danger")
    (gtk:window-set-child window (margins card 24))
    window))
