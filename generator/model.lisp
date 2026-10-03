;;;; model.lisp — typed model of a GIR repository
;;;;
;;;; Mirrors the GIR schema closely; interpretation (what a parameter's
;;;; annotations mean for marshalling) belongs to the planner, not here.

(in-package #:gtk4.generator)

;;; Types

(defstruct (gir-type (:copier nil))
  "A <type> reference: NAME is the GIR name (\"utf8\", \"Gtk.Widget\", \"GLib.List\"),
PARAMS holds element types for containers."
  name c-type params)

(defstruct (gir-array (:copier nil))
  "An <array>: NAME is set for GLib.Array/PtrArray/ByteArray, nil for C arrays.
LENGTH is the index of the parameter holding the length."
  name c-type element length zero-terminated fixed-size)

;;; Members shared by most top-level items

(defstruct (gir-item (:copier nil))
  name doc version deprecated deprecated-version (introspectable t))

;;; Callables

(defstruct (gir-parameter (:copier nil))
  name type direction transfer nullable optional caller-allocates
  closure destroy scope instance-p doc)

(defstruct (gir-callable (:include gir-item) (:copier nil))
  "KIND is one of :function :method :constructor :virtual-method :callback :signal."
  kind c-identifier parameters return-type return-transfer return-nullable
  throws shadows shadowed-by moved-to invoker
  ;; signal-only
  when detailed action no-recurse no-hooks)

;;; Properties, fields, enum members, constants, aliases

(defstruct (gir-property (:include gir-item) (:copier nil))
  type readable writable construct construct-only transfer getter setter)

(defstruct (gir-field (:include gir-item) (:copier nil))
  type readable writable private bits)

(defstruct (gir-member (:copier nil))
  name value c-identifier nick doc)

(defstruct (gir-enum (:include gir-item) (:copier nil))
  "KIND is :enumeration or :bitfield."
  kind c-type glib-type-name get-type error-domain members functions)

(defstruct (gir-constant (:include gir-item) (:copier nil))
  value c-type type)

(defstruct (gir-alias (:include gir-item) (:copier nil))
  c-type type)

;;; Classes, interfaces, records, unions, boxed

(defstruct (gir-class (:include gir-item) (:copier nil))
  "KIND is one of :class :interface :record :union :boxed."
  kind c-type parent abstract final fundamental opaque disguised
  glib-type-name get-type type-struct is-gtype-struct-for
  ref-func unref-func copy-function free-function
  implements prerequisites
  constructors methods functions virtual-methods properties signals fields)

;;; Namespace

(defstruct (gir-namespace (:copier nil))
  name version shared-libraries c-identifier-prefixes c-symbol-prefixes
  includes c-includes packages
  classes enums callbacks functions constants aliases
  source)
