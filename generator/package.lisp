;;;; package.lisp — gtk4 generator package

(defpackage #:gtk4.generator
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   ;; repository
   #:*gir-search-path*
   #:*target-namespaces*
   #:find-gir-file
   #:load-repository
   #:load-targets
   #:parse-gir-file
   #:parse-gir-string
   ;; generation
   #:*m1-targets*
   #:generate
   ;; report
   #:namespace-summary
   #:print-summary))
