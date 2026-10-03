;;;; gtk4-tests.asd

(defsystem "gtk4-tests"
  :description "Test suite for gtk4 and gtk4-generator."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("gtk4" "gtk4-generator" "parachute")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "generator")
               (:file "runtime")
               (:file "objects"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :gtk4-tests)))
