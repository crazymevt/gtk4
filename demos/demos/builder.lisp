;;;; builder.lisp — a user interface described in XML
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Builder.html

(in-package #:gtk4-demo)

(defparameter *builder-ui* "
<interface>
  <object class=\"GtkWindow\" id=\"window\">
    <property name=\"title\">Builder</property>
    <property name=\"default-width\">360</property>
    <child>
      <object class=\"GtkBox\">
        <property name=\"orientation\">vertical</property>
        <property name=\"spacing\">12</property>
        <property name=\"margin-top\">24</property>
        <property name=\"margin-bottom\">24</property>
        <property name=\"margin-start\">24</property>
        <property name=\"margin-end\">24</property>
        <child>
          <object class=\"GtkLabel\" id=\"greeting\">
            <property name=\"label\">This window comes from a .ui description.</property>
            <property name=\"wrap\">true</property>
          </object>
        </child>
        <child>
          <object class=\"GtkEntry\" id=\"name\">
            <property name=\"placeholder-text\">Your name</property>
          </object>
        </child>
        <child>
          <object class=\"GtkButton\" id=\"greet\">
            <property name=\"label\">Greet</property>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

(define-demo builder
    (:title "Builder"
     :category "UI definition"
     :description "GtkBuilder creates widgets from an XML description; gtk:builder-get-object
fetches them by id so Lisp code can connect signals.")
  (let* ((builder (gtk:builder-new-from-string *builder-ui* -1))
         (window (gtk:builder-get-object builder "window"))
         (greeting (gtk:builder-get-object builder "greeting"))
         (name (gtk:builder-get-object builder "name")))
    (gobject:connect (gtk:builder-get-object builder "greet") :clicked
                     (lambda (button)
                       (declare (ignore button))
                       (gtk:label-set-text greeting
                                           (format nil "Hello, ~a!"
                                                   (let ((n (gtk:editable-get-text name)))
                                                     (if (string= n "") "stranger" n))))))
    window))
