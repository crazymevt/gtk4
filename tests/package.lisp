;;;; package.lisp — gtk4 test suite

(defpackage #:gtk4-tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:gen #:gtk4.generator)
                    (#:rt #:gtk4.runtime)))

(in-package #:gtk4-tests)

(define-test gtk4-tests)
