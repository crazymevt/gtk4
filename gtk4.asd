;;;; gtk4.asd — GTK 4 bindings for SBCL

(defsystem "gtk4"
  :description "Complete GTK 4 bindings for SBCL, generated from GObject Introspection."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("gtk4/gio"))

(defsystem "gtk4/runtime"
  :description "Hand-written core: library loading, float traps, main thread, GObject runtime."
  :depends-on ("cffi" "alexandria")
  :pathname "src/runtime/"
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "threads")
               (:file "ffi")
               (:file "handles")
               (:file "gtype")
               (:file "main-loop")
               (:file "gerror")
               (:file "gvalue")
               (:file "object")
               (:file "signals")
               (:file "define")))

;;; Generated bindings (src/generated/, written by gtk4-generator)

(defsystem "gtk4/packages"
  :depends-on ("gtk4/runtime")
  :pathname "src/generated/"
  :components ((:file "packages")))

(defsystem "gtk4/glib"
  :depends-on ("gtk4/packages")
  :pathname "src/generated/"
  :components ((:file "glib")))

(defsystem "gtk4/gobject"
  :depends-on ("gtk4/glib")
  :pathname "src/generated/"
  :components ((:file "gobject")))

(defsystem "gtk4/gmodule"
  :depends-on ("gtk4/glib")
  :pathname "src/generated/"
  :components ((:file "gmodule")))

(defsystem "gtk4/gio"
  :depends-on ("gtk4/gobject" "gtk4/gmodule")
  :pathname "src/generated/"
  :components ((:file "gio")))
