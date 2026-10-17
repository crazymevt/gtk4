;;;; gtk4-generator.asd — GIR parser and code emitter (maintainers only)

(defsystem "gtk4-generator"
  :description "Reads GObject Introspection XML and emits the gtk4 binding sources."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("plump" "alexandria" "docset" "gtk4/runtime")
  :pathname "generator/"
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "parse")
               (:file "overrides")
               (:file "overrides/cairo")
               (:file "overrides/glib")
               (:file "overrides/gdk")
               (:file "overrides/gtk")
               (:file "repository")
               (:file "naming")
               (:file "plan")
               (:file "layout")
               (:file "docs")
               (:file "vfuncs")
               (:file "emit")
               (:file "site")
               (:file "docset")
               (:file "report"))
  :in-order-to ((test-op (test-op "gtk4-generator/tests"))))

(defsystem "gtk4-generator/tests"
  :description "Docset semantic regression checks; run in a fresh generator image."
  :depends-on ("gtk4-generator" "parachute")
  :components ((:file "tests/docset"))
  :perform (test-op (op system)
             (unless (uiop:symbol-call :gtk4-generator/tests :run-tests)
               (error "GTK4 docset regression tests failed."))))
