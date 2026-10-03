;;;; gtk4-generator.asd — GIR parser and code emitter (maintainers only)

(defsystem "gtk4-generator"
  :description "Reads GObject Introspection XML and emits the gtk4 binding sources."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("plump" "alexandria" "gtk4/runtime")
  :pathname "generator/"
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "parse")
               (:file "repository")
               (:file "naming")
               (:file "plan")
               (:file "emit")
               (:file "report")))
