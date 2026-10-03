;;;; gtk4-demo.asd — the demo collection and its browser

(defsystem "gtk4-demo"
  :description "Demos of GTK 4 from Lisp, with a browser to read and run them."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("gtk4" "alexandria")
  :pathname "demos/"
  :serial t
  :components ((:file "package")
               (:file "registry")
               (:module "demos"
                :components ((:file "hello-world")
                             (:file "buttons")
                             (:file "grid-layout")
                             (:file "stack-sidebar")
                             (:file "overlay")
                             (:file "entries")
                             (:file "key-events")
                             (:file "list-filter")
                             (:file "column-view")
                             (:file "flow-box" :depends-on ("list-filter"))
                             (:file "text-view")
                             (:file "css-basics")
                             (:file "dark-mode")
                             (:file "drawing")
                             (:file "paint")
                             (:file "builder")
                             (:file "menus")
                             (:file "dialogs")
                             (:file "progress")
                             (:file "revealer")
                             (:file "clipboard")))
               (:file "browser")))
