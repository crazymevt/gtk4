;;;; gtk4-tests.asd

(defsystem "gtk4-tests"
  :description "Test suite for gtk4 and gtk4-generator."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("gtk4" "gtk4-adwaita" "gtk4-generator" "gtk4-demo" "parachute")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "generator")
               (:file "runtime")
               (:file "objects")
               (:file "generated")
               (:file "callbacks")
               (:file "arrays")
               (:file "containers")
               (:file "gtk")
               (:file "structs")
               (:file "cairo")
               (:file "documentation")
               (:file "subclass")
               (:file "lisp-api")
               (:file "examples")
               (:file "deploy")
               (:file "adwaita")
               (:file "demos"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :gtk4-tests)))

(defsystem "gtk4-tests/stress"
  :description "Stress suite: the M1 no-leak gate. Run with `make stress`."
  :depends-on ("gtk4-tests")
  :pathname "tests/"
  :components ((:file "stress")))
