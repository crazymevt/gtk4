;;;; package.lisp — the gtk4 demo collection

(defpackage #:gtk4-demo
  (:use #:cl)
  (:export
   #:define-demo
   #:demo
   #:demo-name
   #:demo-title
   #:demo-category
   #:demo-description
   #:demo-file
   #:all-demos
   #:find-demo
   #:make-demo-window
   #:main))
