;;;; template-form.lisp — a composite widget from a GtkBuilder template
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Widget.html#building-composite-widgets-from-template-xml

(in-package #:gtk4-demo)

(defclass signup-form (gtk:box)
  ((name :template-child t :reader form-name)
   (email :template-child t :reader form-email)
   (submit :template-child t :reader form-submit)
   (status :template-child t :reader form-status))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispDemoSignupForm")
  (:template "<interface>
  <template class=\"LispDemoSignupForm\" parent=\"GtkBox\">
    <property name=\"orientation\">vertical</property>
    <property name=\"spacing\">8</property>
    <child><object class=\"GtkEntry\" id=\"name\">
      <property name=\"placeholder-text\">Name</property>
      <signal name=\"changed\" handler=\"signup-validate\" object=\"LispDemoSignupForm\"/>
    </object></child>
    <child><object class=\"GtkEntry\" id=\"email\">
      <property name=\"placeholder-text\">Email address</property>
      <property name=\"input-purpose\">email</property>
      <signal name=\"changed\" handler=\"signup-validate\" object=\"LispDemoSignupForm\"/>
    </object></child>
    <child><object class=\"GtkButton\" id=\"submit\">
      <property name=\"label\">Sign up</property>
      <property name=\"sensitive\">false</property>
      <style><class name=\"suggested-action\"/></style>
      <signal name=\"clicked\" handler=\"signup-submit\" object=\"LispDemoSignupForm\"/>
    </object></child>
    <child><object class=\"GtkLabel\" id=\"status\">
      <property name=\"label\">Fill in both fields.</property>
      <style><class name=\"dim-label\"/></style>
    </object></child>
  </template>
</interface>"))

(defun signup-validate (form entry)
  ;; object="LispDemoSignupForm": the form comes first, then the emitter.
  (declare (ignore entry))
  (let ((ok (and (plusp (length (gtk:editable-get-text (form-name form))))
                 (find #\@ (gtk:editable-get-text (form-email form))))))
    (gtk:widget-set-sensitive (form-submit form) (and ok t))
    (gtk:label-set-text (form-status form) (if ok "Ready." "Fill in both fields."))))

(defun signup-submit (form button)
  (declare (ignore button))
  (gtk:label-set-text (form-status form)
                      (format nil "Welcome, ~a!" (gtk:editable-get-text (form-name form)))))

(define-demo template-form
    (:title "Composite template"
     :category "Custom widgets"
     :description "SIGNUP-FORM's children come from a GtkBuilder template given in its class
definition. Slots marked :template-child hold the template's objects, and the template's
signal handlers are Lisp functions in the class's package.")
  (let ((window (make-demo-frame "Composite template" :width 360 :height 260)))
    (gtk:window-set-child window (margins (make-instance 'signup-form) 18))
    window))
