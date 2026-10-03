;;;; generated.lisp — smoke tests calling generated GLib, GObject and Gio bindings

(in-package #:gtk4-tests)

(define-test generated :parent gtk4-tests)

(define-test strings-and-constants :parent generated
  (true (stringp (glib:get-user-name)))
  (true (stringp (glib:get-home-dir)))
  (is = 2 glib:+major-version+)
  (is string= "gpointer" (gobject:type-name (rt:gtype-from-name "gpointer"))))

(define-test out-parameters :parent generated
  ;; g_unichar_get_mirror_char returns a boolean and the mirrored char as an out parameter.
  (is equal '(t 41) (multiple-value-list (glib:unichar-get-mirror-char 40))))

(define-test objects-from-constructors :parent generated
  (let ((file (gio:file-new-for-path "/tmp/gtk4-test-file.txt")))
    (true (typep file 'rt:object))
    (is string= "gtk4-test-file.txt" (gio:file-get-basename file))
    (true (search "gtk4-test-file.txt" (gio:file-get-path file)))))

(define-test errors-become-conditions :parent generated
  (let ((file (gio:file-new-for-path "/nonexistent/gtk4/test")))
    (false (gio:file-query-exists file nil))
    (fail (gio:file-read file nil) rt:glib-error)))

(define-test enums-and-flags :parent generated
  (let ((app (gio:application-new "org.lisp.gtk4.Test" '(:non-unique))))
    (is equal '(:non-unique) (gio:application-get-flags app))
    (is string= "org.lisp.gtk4.Test" (gio:application-get-application-id app))
    (is string= "org.lisp.gtk4.Test" (gio:application-application-id app))
    (setf (gio:application-flags app) '(:non-unique :handles-open))
    (is equal '(:handles-open :non-unique)
        (sort (copy-list (gio:application-get-flags app)) #'string<))))

(define-test generated-classes-and-properties :parent generated
  (let ((action (make-instance 'gio:simple-action :name "save")))
    (true (typep action 'gio:action))
    (is string= "save" (gio:simple-action-name action))
    (gio:simple-action-set-enabled action nil)
    (false (gio:action-get-enabled action))))
