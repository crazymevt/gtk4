;;;; layout.lisp — C struct layouts for GIR records
;;;;
;;;; A record's layout is emitted as a CFFI struct (DEFINE-GSTRUCT), so CFFI
;;;; computes offsets for the platform it runs on. Knowing the layout lets the
;;;; bindings allocate structs for caller-allocated out arguments, pass arrays
;;;; of structs, read and write public fields, and construct value types.

(in-package #:gtk4.generator)

(defstruct layout
  qualified symbol union gtype-name fields
  extras                                ; layouts of anonymous inner unions/records
  depends                               ; struct symbols this one embeds
  accessors                             ; (name slot spec writable bits field)
  constructor                           ; symbol or NIL
  item)

(defparameter *layout-basic-types*
  '(("gboolean" . :boolean) ("gchar" . :char) ("guchar" . :uchar)
    ("gint8" . :int8) ("guint8" . :uint8) ("gint16" . :int16) ("guint16" . :uint16)
    ("gint32" . :int32) ("guint32" . :uint32) ("gint64" . :int64) ("guint64" . :uint64)
    ("gshort" . :short) ("gushort" . :ushort) ("gint" . :int) ("guint" . :uint)
    ("glong" . :long) ("gulong" . :ulong) ("gsize" . :size) ("gssize" . :ssize)
    ("gintptr" . :intptr) ("guintptr" . :uintptr) ("gfloat" . :float) ("gdouble" . :double)
    ("gunichar" . :uint32) ("gunichar2" . :uint16) ("goffset" . :int64)
    ("time_t" . :long) ("off_t" . :int64) ("pid_t" . :int) ("uid_t" . :uint)
    ("gpointer" . :pointer) ("gconstpointer" . :pointer) ("utf8" . :pointer) ("filename" . :pointer))
  "GIR basic types -> the CFFI type of a struct field holding one.")

(defparameter *accessor-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype
    :enum :flags :string :object)
  "Field specs that get accessors. Strings and objects are read-only.")

(defparameter *writable-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype :enum :flags))

(defun layout-of (ctx qualified)
  "The LAYOUT for record QUALIFIED, or a string saying why it has none."
  (multiple-value-bind (v found) (gethash qualified (context-layouts ctx))
    (if found
        v
        (progn
          (setf (gethash qualified (context-layouts ctx)) "recursive layout")
          (setf (gethash qualified (context-layouts ctx))
                (let ((item (lookup ctx qualified)))
                  (plan-layout ctx item qualified (type-symbol qualified))))))))

(defun layout-symbol-p (ctx symbol)
  "True when SYMBOL names a struct with a known layout."
  (gethash symbol (context-struct-symbols ctx)))

(defun record-layout-candidate-p (item)
  (and (gir-class-p item)
       (member (gir-class-kind item) '(:record :union))
       (gir-class-fields item)
       (not (gir-class-is-gtype-struct-for item))))

(defun field-layout-type (ctx type nsname anon-name extras depends)
  "The CFFI type for a field of GIR TYPE, as (VALUES type count), or
(VALUES NIL reason). Anonymous inner unions are planned into EXTRAS, a
cons whose car collects layouts; DEPENDS likewise collects struct symbols."
  (flet ((struct-of (layout)
           (push (layout-symbol layout) (car depends))
           (list (if (layout-union layout) :union :struct) (layout-symbol layout))))
    (typecase type
      (null (values nil "untyped field"))
      (gir-callable :pointer)
      (gir-class
       ;; An anonymous union or record declared inside the struct.
       (let ((inner (plan-layout ctx type nil anon-name)))
         (if (stringp inner)
             (values nil inner)
             (progn (push inner (car extras)) (struct-of inner)))))
      (gir-array
       (if (gir-array-fixed-size type)
           (multiple-value-bind (element count)
               (field-layout-type ctx (gir-array-element type) nsname anon-name extras depends)
             (declare (ignore count))
             (if element
                 (values element (gir-array-fixed-size type))
                 (values nil count)))
           :pointer))
      (gir-type
       (let* ((name (gir-type-name type))
              (c-type (gir-type-c-type type))
              (basic (cdr (assoc name *layout-basic-types* :test #'equal))))
         (cond
           ((equal name "GType") (values (find-symbol "GTYPE" "GTK4.RUNTIME")))
           (basic basic)
           ((and c-type (find #\* c-type)) :pointer)
           ((null name) (values nil "untyped field"))
           (t
            (let* ((qualified (qualify name nsname))
                   (item (lookup ctx qualified)))
              (typecase item
                (null (values nil (format nil "unresolved field type ~a" qualified)))
                (gir-enum (if (eq (gir-enum-kind item) :bitfield) :uint :int))
                (gir-alias (field-layout-type ctx (gir-alias-type item) (namespace-of qualified)
                                              anon-name extras depends))
                (gir-callable :pointer)
                (gir-class
                 (if (member (gir-class-kind item) '(:class :interface))
                     (values nil "embedded object instance")
                     (let ((inner (layout-of ctx qualified)))
                       (if (stringp inner)
                           (values nil (format nil "~a: ~a" qualified inner))
                           (struct-of inner)))))
                (t (values nil "unsupported field type")))))))))))

(defun plan-layout (ctx item qualified symbol)
  "A LAYOUT for record ITEM (QUALIFIED may be NIL for an anonymous inner
union, then SYMBOL is its generated name), or a reason string."
  (cond
    ((not (and (gir-class-p item)
               (member (gir-class-kind item) '(:record :union))
               (not (gir-class-is-gtype-struct-for item))))
     "not a record")
    ((null (gir-class-fields item)) "no fields (opaque)")
    ((null symbol) "no Lisp name")
    (t
     (block plan
       (let* ((nsname (if qualified (namespace-of qualified) (namespace-of (symbol-qualified symbol))))
              (extras (list nil))
              (depends (list nil))
              (fields '())
              (accessors '())
              (unit nil) (unit-bits 0) (unit-count 0))
         (loop for f in (gir-class-fields item)
               for index from 0
               for slot = (intern (string-upcase (snake-to-kebab (gir-item-name f))) :keyword)
               do (if (gir-field-bits f)
                      ;; Bitfields pack into 32-bit units, low bits first (GCC and Clang
                      ;; on the platforms GTK supports).
                      (progn
                        (when (or (null unit) (> (+ unit-bits (gir-field-bits f)) 32))
                          (setf unit (intern (format nil "BITS-~d" unit-count) :keyword)
                                unit-bits 0)
                          (incf unit-count)
                          (push (list unit :uint) fields))
                        (accessor-for ctx f slot (list (gir-field-bits f) unit-bits) unit nsname
                                      (lambda (a) (push a accessors)))
                        (incf unit-bits (gir-field-bits f)))
                      (progn
                        (setf unit nil)
                        (multiple-value-bind (ctype count)
                            (field-layout-type ctx (gir-field-type f) nsname
                                               (intern (format nil "~a-~a-~d" (symbol-name symbol)
                                                               (string-upcase (snake-to-kebab (gir-item-name f)))
                                                               index)
                                                       (symbol-package symbol))
                                               extras depends)
                          (unless ctype (return-from plan count))
                          (push (if count (list slot ctype :count count) (list slot ctype)) fields)
                          (unless count
                            (accessor-for ctx f slot nil slot nsname
                                          (lambda (a) (push a accessors))
                                          (and (consp ctype) (member (first ctype) '(:struct :union)))))))))
         (make-layout :qualified qualified :symbol symbol
                      :union (eq (gir-class-kind item) :union)
                      :gtype-name (and qualified (gir-class-get-type item)
                                       (not (member qualified *pointer-records* :test #'string=))
                                       (gir-class-glib-type-name item))
                      :fields (nreverse fields)
                      :extras (reverse (car extras))
                      :depends (remove-duplicates (car depends))
                      :accessors (nreverse accessors)
                      :item item))))))

(defun symbol-qualified (symbol)
  "The GIR namespace name for SYMBOL's package, as a qualified-name prefix."
  (let ((pkg (package-name (symbol-package symbol))))
    (format nil "~a.~a" (or (car (rassoc pkg *namespace-packages* :test #'string=)) pkg)
            (symbol-name symbol))))

(defun accessor-for (ctx field slot bits unit-slot nsname collect &optional inline)
  "Record an accessor for FIELD when it is public and of a supported kind.
INLINE means the field embeds a struct (read as a copy, written by copying).
Names are claimed later, in PLAN-NAMESPACE-LAYOUTS."
  (when (and (not (gir-field-private field))
             (gir-field-readable field)
             (gir-type-p (gir-field-type field)))
    (multiple-value-bind (spec why) (classify-type ctx (gir-field-type field) nsname)
      (declare (ignore why))
      (cond
        ((and spec inline (member (spec-kind* spec) '(:boxed :record))
              (if (eq (spec-kind* spec) :boxed) (fourth spec) (layout-symbol-p ctx (second spec))))
         (funcall collect
                  (list :name nil :slot unit-slot :spec spec
                        :writable (and (gir-field-writable field) t)
                        :inline t :field field)))
        ((and spec (not inline) (member (spec-kind* spec) *accessor-kinds*))
         (funcall collect
                  (list :name nil :slot unit-slot :spec spec
                        :writable (and (gir-field-writable field)
                                       (member (spec-kind* spec) *writable-kinds*)
                                       t)
                        :bits (and bits slot bits)
                        :field field)))))))

;;; Planning and naming every layout in a namespace (phase 1)

(defun plan-namespace-layouts (ctx ns package)
  "Plan layouts for NS's records, register their struct symbols, and claim
accessor and constructor names in PACKAGE."
  (let ((nsname (gir-namespace-name ns)))
    (dolist (item (gir-namespace-classes ns))
      (when (record-layout-candidate-p item)
        (let ((layout (layout-of ctx (qualify (gir-item-name item) nsname))))
          (unless (stringp layout)
            (labels ((register (l)
                       (setf (gethash (layout-symbol l) (context-struct-symbols ctx)) l)
                       (mapc #'register (layout-extras l))))
              (register layout))
            (let ((kebab (camel-to-kebab (gir-item-name item))))
              (dolist (a (layout-accessors layout))
                (setf (getf a :name)
                      (claim-symbol ctx package
                                    (format nil "~a-~a" kebab
                                            (snake-to-kebab (gir-item-name (getf a :field))))
                                    (format nil "~a.~a field" (gir-item-name item)
                                            (gir-item-name (getf a :field))))))
              ;; ACCESSORS holds plists; rewrite in place with names filled in.
              (setf (layout-accessors layout)
                    (remove nil (layout-accessors layout) :key (lambda (a) (getf a :name))))
              (when (some (lambda (a) (getf a :writable)) (layout-accessors layout))
                (setf (layout-constructor layout)
                      (claim-symbol ctx package (format nil "make-~a" kebab)
                                    (format nil "~a constructor" (gir-item-name item))))))))))))

(defun namespace-layouts-in-order (ctx ns)
  "NS's layouts (with their anonymous inner layouts) ordered so each follows
any same-namespace struct it embeds."
  (let ((nsname (gir-namespace-name ns))
        (order '())
        (seen (make-hash-table :test 'eq)))
    (labels ((visit (l)
               (unless (gethash l seen)
                 (setf (gethash l seen) t)
                 (dolist (sym (layout-depends l))
                   (let ((dep (gethash sym (context-struct-symbols ctx))))
                     (when (and dep (eq (symbol-package sym) (symbol-package (layout-symbol l))))
                       (visit dep))))
                 (push l order))))
      (dolist (item (gir-namespace-classes ns))
        (when (record-layout-candidate-p item)
          (let ((l (gethash (qualify (gir-item-name item) nsname) (context-layouts ctx))))
            (when (layout-p l) (visit l))))))
    (nreverse order)))
