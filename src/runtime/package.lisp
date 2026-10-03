;;;; package.lisp — gtk4 runtime package

(defpackage #:gtk4.runtime
  (:use #:cl)
  (:export
   ;; libraries
   #:*library-directories*
   #:+minimum-gtk-version+
   #:load-libraries
   #:library-loaded-p
   #:gtk-version
   #:gtk-version>=
   ;; threads
   #:with-gtk-float-traps
   #:main-thread-p
   #:check-main-thread
   #:wrong-thread-error))
