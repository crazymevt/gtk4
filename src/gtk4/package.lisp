;;;; package.lisp — the gtk4 package: the idiomatic layer above the
;;;; generated bindings. For now it holds the documentation helpers.

(defpackage #:gtk4
  (:use #:cl)
  (:import-from #:gtk4.runtime
                #:c-name #:lisp-name #:documentation-url #:browse)
  (:export
   ;; Moving between the Lisp API and GTK's documentation
   #:c-name
   #:lisp-name
   #:documentation-url
   #:browse))
