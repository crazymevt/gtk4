;;;; gtk4.asd — GTK 4 bindings for SBCL

(defsystem "gtk4"
  :description "Complete GTK 4 bindings for SBCL, generated from GObject Introspection."
  :author "Jessie Hughart"
  :license "MIT"
  :version "1.0.1"
  :homepage "https://github.com/crazymevt/gtk4"
  :source-control (:git "https://github.com/crazymevt/gtk4.git")
  :bug-tracker "https://github.com/crazymevt/gtk4/issues"
  :long-description "Bindings for the whole GTK 4 stack (GLib, GObject, GIO, Pango, cairo,
Graphene, GDK, GSK, GTK), generated from GObject Introspection data, with
docstrings linking every function to its upstream documentation; Lisp-defined
GObject classes with virtual functions, properties, signals and templates;
and a Lisp layer for list models, widget trees, CSS and asynchronous calls."
  :depends-on ("gtk4/gtk")
  :pathname "src/gtk4/"
  :serial t
  :components ((:file "package")
               (:file "templates")
               (:file "models")
               (:file "async")
               (:file "build")
               (:file "css")
               (:file "deploy")))

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
               (:file "define")
               (:file "subclass")
               (:file "properties")
               (:file "image")))

;;; Generated bindings: one system per GIR namespace, each split into files
;;; of a few hundred functions to bound compile-time memory.

;;; BEGIN GENERATED SYSTEMS (written by gtk4-generator; do not edit)

(defsystem "gtk4/packages"
  :depends-on ("gtk4/runtime")
  :pathname "src/generated/"
  :components ((:file "packages")))

(defsystem "gtk4/glib"
  :depends-on ("gtk4/packages")
  :pathname "src/generated/"
  :serial t
  :components ((:file "glib")
               (:file "glib-functions-1")
               (:file "glib-functions-2")
               (:file "glib-functions-3")
               (:file "glib-functions-4")))

(defsystem "gtk4/gobject"
  :depends-on ("gtk4/glib")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gobject")
               (:file "gobject-functions-1")))

(defsystem "gtk4/gmodule"
  :depends-on ("gtk4/glib")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gmodule")
               (:file "gmodule-functions-1")))

(defsystem "gtk4/gio"
  :depends-on ("gtk4/glib" "gtk4/gmodule" "gtk4/gobject")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gio")
               (:file "gio-functions-1")
               (:file "gio-functions-2")
               (:file "gio-functions-3")
               (:file "gio-functions-4")
               (:file "gio-functions-5")
               (:file "gio-functions-6")))

(defsystem "gtk4/cairo"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :serial t
  :components ((:file "cairo")
               (:file "cairo-functions-1")))

(defsystem "gtk4/harfbuzz"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :serial t
  :components ((:file "harfbuzz")
               (:file "harfbuzz-functions-1")
               (:file "harfbuzz-functions-2")))

(defsystem "gtk4/pango"
  :depends-on ("gtk4/gobject" "gtk4/gio" "gtk4/harfbuzz" "gtk4/cairo")
  :pathname "src/generated/"
  :serial t
  :components ((:file "pango")
               (:file "pango-functions-1")
               (:file "pango-functions-2")))

(defsystem "gtk4/pango-cairo"
  :depends-on ("gtk4/gobject" "gtk4/pango" "gtk4/cairo")
  :pathname "src/generated/"
  :serial t
  :components ((:file "pango-cairo")
               (:file "pango-cairo-functions-1")))

(defsystem "gtk4/graphene"
  :depends-on ("gtk4/gobject")
  :pathname "src/generated/"
  :serial t
  :components ((:file "graphene")
               (:file "graphene-functions-1")))

(defsystem "gtk4/gdk-pixbuf"
  :depends-on ("gtk4/gmodule" "gtk4/gio")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gdk-pixbuf")
               (:file "gdk-pixbuf-functions-1")))

(defsystem "gtk4/gdk"
  :depends-on ("gtk4/gdk-pixbuf" "gtk4/gio" "gtk4/pango" "gtk4/pango-cairo" "gtk4/cairo")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gdk")
               (:file "gdk-functions-1")
               (:file "gdk-functions-2")))

(defsystem "gtk4/gsk"
  :depends-on ("gtk4/gdk" "gtk4/graphene")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gsk")
               (:file "gsk-functions-1")))

(defsystem "gtk4/gtk"
  :depends-on ("gtk4/gdk" "gtk4/gsk")
  :pathname "src/generated/"
  :serial t
  :components ((:file "gtk")
               (:file "gtk-functions-1")
               (:file "gtk-functions-2")
               (:file "gtk-functions-3")
               (:file "gtk-functions-4")
               (:file "gtk-functions-5")
               (:file "gtk-functions-6")
               (:file "gtk-functions-7")
               (:file "gtk-functions-8")
               (:file "gtk-functions-9")
               (:file "gtk-functions-10")
               (:file "gtk-functions-11")))

(defsystem "gtk4/adw"
  :depends-on ("gtk4/gio" "gtk4/gtk")
  :pathname "src/generated/"
  :serial t
  :components ((:file "adw")
               (:file "adw-functions-1")
               (:file "adw-functions-2")
               (:file "adw-functions-3")
               (:file "adw-functions-4")))

;;; END GENERATED SYSTEMS
