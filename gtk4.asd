;;;; gtk4.asd — GTK 4 bindings for SBCL

(defsystem "gtk4"
  :description "Complete GTK 4 bindings for SBCL, generated from GObject Introspection."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("gtk4/gtk"))

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

(defsystem "gtk4/cairo"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :components ((:file "cairo")))

(defsystem "gtk4/harfbuzz"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :components ((:file "harfbuzz")))

(defsystem "gtk4/pango"
  :depends-on ("gtk4/gio" "gtk4/harfbuzz" "gtk4/cairo")
  :pathname "src/generated/"
  :components ((:file "pango")))

(defsystem "gtk4/pango-cairo"
  :depends-on ("gtk4/pango" "gtk4/cairo")
  :pathname "src/generated/"
  :components ((:file "pango-cairo")))

(defsystem "gtk4/graphene"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :components ((:file "graphene")))

(defsystem "gtk4/gdk-pixbuf"
  :depends-on ("gtk4/gio" "gtk4/gmodule")
  :pathname "src/generated/"
  :components ((:file "gdk-pixbuf")))

(defsystem "gtk4/gdk"
  :depends-on ("gtk4/gdk-pixbuf" "gtk4/gio" "gtk4/pango" "gtk4/pango-cairo" "gtk4/cairo")
  :pathname "src/generated/"
  :components ((:file "gdk")))

(defsystem "gtk4/gsk"
  :depends-on ("gtk4/gdk" "gtk4/graphene")
  :pathname "src/generated/"
  :components ((:file "gsk")))

(defsystem "gtk4/gtk"
  :depends-on ("gtk4/gdk" "gtk4/gsk")
  :pathname "src/generated/"
  :components ((:file "gtk")))
