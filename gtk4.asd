;;;; gtk4.asd — GTK 4 bindings for SBCL

(defsystem "gtk4"
  :description "Complete GTK 4 bindings for SBCL, generated from GObject Introspection."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("gtk4/runtime"))

(defsystem "gtk4/runtime"
  :description "Hand-written core: library loading, float traps, main thread, GObject runtime."
  :depends-on ("cffi" "alexandria")
  :pathname "src/runtime/"
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "threads")))
