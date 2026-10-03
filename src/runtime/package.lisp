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
   ;; threads and the main loop
   #:with-gtk-float-traps
   #:main-thread-p
   #:check-main-thread
   #:wrong-thread-error
   #:*callback-error-handler*
   #:with-callback-protection
   #:*gui-thread*
   #:gui-thread-p
   #:call-in-main-thread
   #:in-main-thread
   #:iterate-main-context
   ;; handles
   #:make-handle
   #:handle-value
   #:free-handle
   #:handle-count
   ;; GType
   #:gtype
   #:gtype-name
   #:gtype-from-name
   #:gtype-parent
   #:gtype-fundamental
   #:gtype-is-a
   #:instance-gtype
   ;; GError
   #:glib-error
   #:glib-error-domain
   #:glib-error-code
   #:glib-error-message
   #:*error-domain-conditions*
   #:with-gerror
   #:define-gerror-domain
   #:glib-error-keyword
   ;; GValue
   #:with-gvalue
   #:gvalue-get
   #:set-gvalue
   #:register-enum-converter
   ;; objects
   #:gobject-class
   #:class-gtype
   #:object
   #:initially-unowned
   #:boxed
   #:boxed-gtype
   #:*boxed-classes*
   #:object-pointer
   #:wrap-object
   #:wrap-boxed
   #:proxy-count
   #:property
   ;; Lisp-defined GTypes
   #:define-vfunc
   #:call-next-vfunc
   #:remove-vfunc
   #:call-vfunc
   #:find-vfunc
   #:template-child
   #:designator-gtype
   #:*class-init-hooks*
   #:*instance-init-hooks*
   ;; signals
   #:connect
   #:disconnect
   #:block-handler
   #:unblock-handler
   #:handler-connected-p
   #:emit
   #:make-closure
   ;; definitions used by generated code
   #:define-gfunction
   #:define-gvfunc
   #:define-async
   #:async-finish-info
   #:define-gcallback
   #:define-genum
   #:define-gconstant
   #:define-gclass
   #:define-grecord
   #:define-gproperty
   #:define-gstruct
   #:define-gfield
   #:define-gstruct-constructor
   #:record
   #:unavailable-function
   #:c-name
   #:lisp-name
   #:documentation-url
   #:browse
   #:enum-value
   #:enum-keyword))
