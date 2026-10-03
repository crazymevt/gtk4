;;;; generator.lisp — GIR parser tests

(in-package #:gtk4-tests)

(define-test generator :parent gtk4-tests)

(defparameter *sample-gir* "<?xml version=\"1.0\"?>
<repository version=\"1.2\" xmlns=\"http://www.gtk.org/introspection/core/1.0\"
            xmlns:c=\"http://www.gtk.org/introspection/c/1.0\"
            xmlns:glib=\"http://www.gtk.org/introspection/glib/1.0\">
  <include name=\"GObject\" version=\"2.0\"/>
  <package name=\"sample\"/>
  <namespace name=\"Smp\" version=\"1.0\" shared-library=\"libsmp.so.1,libsmp-extra.so.1\"
             c:identifier-prefixes=\"Smp\" c:symbol-prefixes=\"smp\">
    <class name=\"Widget\" c:type=\"SmpWidget\" parent=\"GObject.Object\" abstract=\"1\"
           glib:type-name=\"SmpWidget\" glib:get-type=\"smp_widget_get_type\">
      <doc xml:space=\"preserve\">A widget &amp; friends.</doc>
      <implements name=\"Gio.ListModel\"/>
      <constructor name=\"new\" c:identifier=\"smp_widget_new\">
        <return-value transfer-ownership=\"none\"><type name=\"Widget\" c:type=\"SmpWidget*\"/></return-value>
      </constructor>
      <method name=\"set_items\" c:identifier=\"smp_widget_set_items\" throws=\"1\" version=\"1.2\">
        <return-value transfer-ownership=\"none\"><type name=\"gboolean\" c:type=\"gboolean\"/></return-value>
        <parameters>
          <instance-parameter name=\"self\" transfer-ownership=\"none\"><type name=\"Widget\" c:type=\"SmpWidget*\"/></instance-parameter>
          <parameter name=\"items\" transfer-ownership=\"none\" nullable=\"1\">
            <array length=\"1\" zero-terminated=\"0\" c:type=\"const char**\"><type name=\"utf8\" c:type=\"char*\"/></array>
          </parameter>
          <parameter name=\"n_items\" transfer-ownership=\"none\"><type name=\"gsize\" c:type=\"gsize\"/></parameter>
          <parameter name=\"min\" direction=\"out\" caller-allocates=\"0\" transfer-ownership=\"full\"><type name=\"gint\" c:type=\"int*\"/></parameter>
        </parameters>
      </method>
      <method name=\"printf\" c:identifier=\"smp_widget_printf\" introspectable=\"0\">
        <return-value><type name=\"none\" c:type=\"void\"/></return-value>
        <parameters>
          <instance-parameter name=\"self\"><type name=\"Widget\" c:type=\"SmpWidget*\"/></instance-parameter>
          <parameter name=\"...\"><varargs/></parameter>
        </parameters>
      </method>
      <virtual-method name=\"snapshot\">
        <return-value><type name=\"none\" c:type=\"void\"/></return-value>
      </virtual-method>
      <property name=\"label\" writable=\"1\" construct-only=\"1\" transfer-ownership=\"none\" getter=\"get_label\">
        <type name=\"utf8\" c:type=\"gchar*\"/>
      </property>
      <glib:signal name=\"changed\" when=\"last\" detailed=\"1\">
        <return-value transfer-ownership=\"none\"><type name=\"none\" c:type=\"void\"/></return-value>
        <parameters>
          <parameter name=\"names\" transfer-ownership=\"none\">
            <type name=\"GLib.List\" c:type=\"GList*\"><type name=\"utf8\"/></type>
          </parameter>
        </parameters>
      </glib:signal>
    </class>
    <bitfield name=\"Flags\" c:type=\"SmpFlags\" glib:type-name=\"SmpFlags\">
      <member name=\"none\" value=\"0\" c:identifier=\"SMP_FLAGS_NONE\" glib:nick=\"none\"/>
      <member name=\"expand\" value=\"2\" c:identifier=\"SMP_FLAGS_EXPAND\" glib:nick=\"expand\"/>
    </bitfield>
    <function name=\"init\" c:identifier=\"smp_init\">
      <return-value><type name=\"none\" c:type=\"void\"/></return-value>
    </function>
    <constant name=\"MAJOR\" value=\"1\" c:type=\"SMP_MAJOR\"><type name=\"gint\" c:type=\"gint\"/></constant>
  </namespace>
</repository>")

(defun sample () (gen:parse-gir-string *sample-gir*))

(defun find-named (name list)
  (find name list :key #'gen::gir-item-name :test #'string=))

(define-test namespace-header :parent generator
  (let ((ns (sample)))
    (is string= "Smp" (gen::gir-namespace-name ns))
    (is string= "1.0" (gen::gir-namespace-version ns))
    (is equal '("libsmp.so.1" "libsmp-extra.so.1") (gen::gir-namespace-shared-libraries ns))
    (is equal '(("GObject" "2.0")) (gen::gir-namespace-includes ns))
    (is equal '("sample") (gen::gir-namespace-packages ns))))

(define-test class-shape :parent generator
  (let ((w (first (gen::gir-namespace-classes (sample)))))
    (is eq :class (gen::gir-class-kind w))
    (is string= "GObject.Object" (gen::gir-class-parent w))
    (true (gen::gir-class-abstract w))
    (is string= "smp_widget_get_type" (gen::gir-class-get-type w))
    (is equal '("Gio.ListModel") (gen::gir-class-implements w))
    (is string= "A widget & friends." (gen::gir-item-doc w))
    (is = 1 (length (gen::gir-class-constructors w)))
    (is = 2 (length (gen::gir-class-methods w)))
    (is = 1 (length (gen::gir-class-virtual-methods w)))))

(define-test method-parameters :parent generator
  (let* ((w (first (gen::gir-namespace-classes (sample))))
         (m (find-named "set_items" (gen::gir-class-methods w)))
         (params (gen::gir-callable-parameters m)))
    (true (gen::gir-callable-throws m))
    (is string= "1.2" (gen::gir-item-version m))
    (is = 4 (length params))
    (true (gen::gir-parameter-instance-p (first params)))
    (let* ((items (second params))
           (array (gen::gir-parameter-type items)))
      (true (gen::gir-parameter-nullable items))
      (true (gen::gir-array-p array))
      (is = 1 (gen::gir-array-length array))
      (false (gen::gir-array-zero-terminated array))
      (is string= "utf8" (gen::gir-type-name (gen::gir-array-element array))))
    (let ((out (fourth params)))
      (is eq :out (gen::gir-parameter-direction out))
      (is eq :full (gen::gir-parameter-transfer out)))))

(define-test varargs-and-introspectable :parent generator
  (let* ((w (first (gen::gir-namespace-classes (sample))))
         (m (find-named "printf" (gen::gir-class-methods w))))
    (false (gen::gir-item-introspectable m))
    (is eq :varargs (gen::gir-parameter-type (second (gen::gir-callable-parameters m))))))

(define-test properties-and-signals :parent generator
  (let* ((w (first (gen::gir-namespace-classes (sample))))
         (p (first (gen::gir-class-properties w)))
         (s (first (gen::gir-class-signals w))))
    (is string= "label" (gen::gir-item-name p))
    (true (gen::gir-property-readable p))
    (true (gen::gir-property-construct-only p))
    (is string= "get_label" (gen::gir-property-getter p))
    (is eq :signal (gen::gir-callable-kind s))
    (is eq :last (gen::gir-callable-when s))
    (true (gen::gir-callable-detailed s))
    (let ((list-type (gen::gir-parameter-type (first (gen::gir-callable-parameters s)))))
      (is string= "GLib.List" (gen::gir-type-name list-type))
      (is string= "utf8" (gen::gir-type-name (first (gen::gir-type-params list-type)))))))

(define-test enums-functions-constants :parent generator
  (let* ((ns (sample))
         (flags (first (gen::gir-namespace-enums ns))))
    (is eq :bitfield (gen::gir-enum-kind flags))
    (is equal '(0 2) (mapcar #'gen::gir-member-value (gen::gir-enum-members flags)))
    (is string= "smp_init" (gen::gir-callable-c-identifier (first (gen::gir-namespace-functions ns))))
    (is string= "1" (gen::gir-constant-value (first (gen::gir-namespace-constants ns))))))

(define-test installed-gtk-gir :parent generator
  ;; Parses the real Gtk-4.0.gir when it is installed.
  (let ((path (gen:find-gir-file "Gtk" "4.0")))
    (if (null path)
        (skip "Gtk-4.0.gir not installed" (true t))
        (let ((ns (gen:parse-gir-file path)))
          (is string= "Gtk" (gen::gir-namespace-name ns))
          (true (find-named "Widget" (gen::gir-namespace-classes ns)))
          (true (< 3000 (getf (gen:namespace-summary ns) :callables)))))))
