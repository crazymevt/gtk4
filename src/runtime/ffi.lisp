;;;; ffi.lisp — the C declarations the runtime itself needs
;;;;
;;;; Hand-written because the runtime must exist before any generated code
;;;; loads. Everything user-facing is generated; these stay internal.

(in-package #:gtk4.runtime)

(cffi:defctype gtype :size)

;;; Structures whose layout the runtime reads directly. All are public,
;;; ABI-stable GLib structs.

(cffi:defcstruct gtype-instance (g-class :pointer))
(cffi:defcstruct gtype-class (g-type gtype))

(cffi:defcstruct gvalue
  (g-type gtype)
  (data :uint64 :count 2))

(cffi:defcstruct gparam-spec
  (g-type-instance :pointer)
  (name :pointer)
  (flags :int)
  (value-type gtype)
  (owner-type gtype))

(cffi:defcstruct gclosure
  (flags :uint)                         ; ref_count:15, meta_marshal_nouse:1, ... bitfields
  (marshal :pointer)
  (data :pointer)
  (notifiers :pointer))

(cffi:defcstruct gerror
  (domain :uint32)
  (code :int)
  (message :pointer))

(defconstant +gvalue-size+ (cffi:foreign-type-size '(:struct gvalue)))

;;; Fundamental type ids (G_TYPE_MAKE_FUNDAMENTAL (n) = n << 2)

(defconstant +g-type-invalid+ 0)
(defconstant +g-type-none+ 4)
(defconstant +g-type-interface+ 8)
(defconstant +g-type-char+ 12)
(defconstant +g-type-uchar+ 16)
(defconstant +g-type-boolean+ 20)
(defconstant +g-type-int+ 24)
(defconstant +g-type-uint+ 28)
(defconstant +g-type-long+ 32)
(defconstant +g-type-ulong+ 36)
(defconstant +g-type-int64+ 40)
(defconstant +g-type-uint64+ 44)
(defconstant +g-type-enum+ 48)
(defconstant +g-type-flags+ 52)
(defconstant +g-type-float+ 56)
(defconstant +g-type-double+ 60)
(defconstant +g-type-string+ 64)
(defconstant +g-type-pointer+ 68)
(defconstant +g-type-boxed+ 72)
(defconstant +g-type-param+ 76)
(defconstant +g-type-object+ 80)
(defconstant +g-type-variant+ 84)

;;; GLib

(cffi:defcfun ("g_free" %g-free) :void (mem :pointer))
(cffi:defcfun ("g_quark_to_string" %g-quark-to-string) :string (quark :uint32))
(cffi:defcfun ("g_error_free" %g-error-free) :void (error :pointer))
(cffi:defcfun ("g_idle_add" %g-idle-add) :uint (fn :pointer) (data :pointer))
(cffi:defcfun ("g_idle_add_full" %g-idle-add-full) :uint
  (priority :int) (fn :pointer) (data :pointer) (notify :pointer))
(cffi:defcfun ("g_main_context_iteration" %g-main-context-iteration) :boolean
  (context :pointer) (may-block :boolean))
(cffi:defcfun ("g_main_context_pending" %g-main-context-pending) :boolean (context :pointer))

;;; GType

(cffi:defcfun ("g_type_name" %g-type-name) :string (type gtype))
(cffi:defcfun ("g_type_from_name" %g-type-from-name) gtype (name :string))
(cffi:defcfun ("g_type_parent" %g-type-parent) gtype (type gtype))
(cffi:defcfun ("g_type_fundamental" %g-type-fundamental) gtype (type gtype))
(cffi:defcfun ("g_type_is_a" %g-type-is-a) :boolean (type gtype) (is-a-type gtype))
(cffi:defcfun ("g_type_class_ref" %g-type-class-ref) :pointer (type gtype))
(cffi:defcfun ("g_gtype_get_type" %g-gtype-get-type) gtype)

;;; GObject

(cffi:defcfun ("g_object_ref" %g-object-ref) :pointer (object :pointer))
(cffi:defcfun ("g_object_unref" %g-object-unref) :void (object :pointer))
(cffi:defcfun ("g_object_ref_sink" %g-object-ref-sink) :pointer (object :pointer))
(cffi:defcfun ("g_object_is_floating" %g-object-is-floating) :boolean (object :pointer))
(cffi:defcfun ("g_object_new_with_properties" %g-object-new-with-properties) :pointer
  (type gtype) (n-properties :uint) (names :pointer) (values :pointer))
(cffi:defcfun ("g_object_class_find_property" %g-object-class-find-property) :pointer
  (class :pointer) (name :string))
(cffi:defcfun ("g_object_get_property" %g-object-get-property) :void
  (object :pointer) (name :string) (value :pointer))
(cffi:defcfun ("g_object_set_property" %g-object-set-property) :void
  (object :pointer) (name :string) (value :pointer))
(cffi:defcfun ("g_object_weak_ref" %g-object-weak-ref) :void
  (object :pointer) (notify :pointer) (data :pointer))

;;; Boxed

(cffi:defcfun ("g_boxed_copy" %g-boxed-copy) :pointer (type gtype) (boxed :pointer))
(cffi:defcfun ("g_boxed_free" %g-boxed-free) :void (type gtype) (boxed :pointer))

;;; GValue

(cffi:defcfun ("g_value_init" %g-value-init) :pointer (value :pointer) (type gtype))
(cffi:defcfun ("g_value_unset" %g-value-unset) :void (value :pointer))
(cffi:defcfun ("g_value_get_boolean" %g-value-get-boolean) :boolean (value :pointer))
(cffi:defcfun ("g_value_set_boolean" %g-value-set-boolean) :void (value :pointer) (v :boolean))
(cffi:defcfun ("g_value_get_schar" %g-value-get-schar) :int8 (value :pointer))
(cffi:defcfun ("g_value_set_schar" %g-value-set-schar) :void (value :pointer) (v :int8))
(cffi:defcfun ("g_value_get_uchar" %g-value-get-uchar) :uint8 (value :pointer))
(cffi:defcfun ("g_value_set_uchar" %g-value-set-uchar) :void (value :pointer) (v :uint8))
(cffi:defcfun ("g_value_get_int" %g-value-get-int) :int (value :pointer))
(cffi:defcfun ("g_value_set_int" %g-value-set-int) :void (value :pointer) (v :int))
(cffi:defcfun ("g_value_get_uint" %g-value-get-uint) :uint (value :pointer))
(cffi:defcfun ("g_value_set_uint" %g-value-set-uint) :void (value :pointer) (v :uint))
(cffi:defcfun ("g_value_get_long" %g-value-get-long) :long (value :pointer))
(cffi:defcfun ("g_value_set_long" %g-value-set-long) :void (value :pointer) (v :long))
(cffi:defcfun ("g_value_get_ulong" %g-value-get-ulong) :ulong (value :pointer))
(cffi:defcfun ("g_value_set_ulong" %g-value-set-ulong) :void (value :pointer) (v :ulong))
(cffi:defcfun ("g_value_get_int64" %g-value-get-int64) :int64 (value :pointer))
(cffi:defcfun ("g_value_set_int64" %g-value-set-int64) :void (value :pointer) (v :int64))
(cffi:defcfun ("g_value_get_uint64" %g-value-get-uint64) :uint64 (value :pointer))
(cffi:defcfun ("g_value_set_uint64" %g-value-set-uint64) :void (value :pointer) (v :uint64))
(cffi:defcfun ("g_value_get_float" %g-value-get-float) :float (value :pointer))
(cffi:defcfun ("g_value_set_float" %g-value-set-float) :void (value :pointer) (v :float))
(cffi:defcfun ("g_value_get_double" %g-value-get-double) :double (value :pointer))
(cffi:defcfun ("g_value_set_double" %g-value-set-double) :void (value :pointer) (v :double))
(cffi:defcfun ("g_value_get_enum" %g-value-get-enum) :int (value :pointer))
(cffi:defcfun ("g_value_set_enum" %g-value-set-enum) :void (value :pointer) (v :int))
(cffi:defcfun ("g_value_get_flags" %g-value-get-flags) :uint (value :pointer))
(cffi:defcfun ("g_value_set_flags" %g-value-set-flags) :void (value :pointer) (v :uint))
(cffi:defcfun ("g_value_get_string" %g-value-get-string) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_string" %g-value-set-string) :void (value :pointer) (v :string))
(cffi:defcfun ("g_value_get_pointer" %g-value-get-pointer) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_pointer" %g-value-set-pointer) :void (value :pointer) (v :pointer))
(cffi:defcfun ("g_value_get_boxed" %g-value-get-boxed) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_boxed" %g-value-set-boxed) :void (value :pointer) (v :pointer))
(cffi:defcfun ("g_value_get_object" %g-value-get-object) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_object" %g-value-set-object) :void (value :pointer) (v :pointer))
(cffi:defcfun ("g_value_get_gtype" %g-value-get-gtype) gtype (value :pointer))
(cffi:defcfun ("g_value_set_gtype" %g-value-set-gtype) :void (value :pointer) (v gtype))
(cffi:defcfun ("g_value_get_variant" %g-value-get-variant) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_variant" %g-value-set-variant) :void (value :pointer) (v :pointer))
(cffi:defcfun ("g_value_get_param" %g-value-get-param) :pointer (value :pointer))
(cffi:defcfun ("g_value_set_param" %g-value-set-param) :void (value :pointer) (v :pointer))

;;; Signals and closures

(cffi:defcfun ("g_closure_new_simple" %g-closure-new-simple) :pointer
  (sizeof-closure :uint) (data :pointer))
(cffi:defcfun ("g_closure_set_marshal" %g-closure-set-marshal) :void
  (closure :pointer) (marshal :pointer))
(cffi:defcfun ("g_closure_add_finalize_notifier" %g-closure-add-finalize-notifier) :void
  (closure :pointer) (data :pointer) (notify :pointer))
(cffi:defcfun ("g_signal_connect_closure" %g-signal-connect-closure) :ulong
  (instance :pointer) (detailed-signal :string) (closure :pointer) (after :boolean))
(cffi:defcfun ("g_signal_handler_disconnect" %g-signal-handler-disconnect) :void
  (instance :pointer) (handler-id :ulong))
(cffi:defcfun ("g_signal_handler_block" %g-signal-handler-block) :void
  (instance :pointer) (handler-id :ulong))
(cffi:defcfun ("g_signal_handler_unblock" %g-signal-handler-unblock) :void
  (instance :pointer) (handler-id :ulong))
(cffi:defcfun ("g_signal_handler_is_connected" %g-signal-handler-is-connected) :boolean
  (instance :pointer) (handler-id :ulong))
