;;;; gtk4-adwaita.asd — libadwaita bindings for SBCL (optional)

(defsystem "gtk4-adwaita"
  :description "libadwaita bindings: the ADW package, generated from Adw-1.gir, plus a few helpers."
  :author "Jessie Hughart"
  :license "MIT"
  :version "1.0.1"
  :depends-on ("gtk4" "gtk4/adw")
  :pathname "src/adwaita/"
  :components ((:file "helpers")))
