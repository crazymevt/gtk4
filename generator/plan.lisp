;;;; plan.lisp — deciding how each GIR item becomes Lisp
;;;;
;;;; Phase 1 (NAME-NAMESPACES) gives every item in every target namespace a
;;;; Lisp symbol, creating the packages, so cross-namespace references print
;;;; correctly. Phase 2 (PLAN-CALLABLE) turns each callable into the argument
;;;; specs DEFINE-GFUNCTION understands, or a reason it cannot be bound yet.

(in-package #:gtk4.generator)

;;; The generation context

(defstruct (context (:constructor %make-context))
  repository                            ; "Name-Version" -> gir-namespace
  targets                               ; list of gir-namespace, in load order
  (index (make-hash-table :test 'equal)) ; "Ns.Name" -> gir item
  (owners (make-hash-table :test 'equal)) ; package/symbol-name -> description, for collisions
  (skipped (make-hash-table :test 'equal)) ; namespace -> list of (what . reason)
  (bound (make-hash-table :test 'equal))   ; namespace -> count of callables bound
  (callback-plans (make-hash-table :test 'equal))) ; "Ns.Name" -> (args return transfer) or reason string

(defun target-namespaces-in-order (repository targets)
  "TARGETS as gir-namespaces, ordered so every namespace follows its includes."
  (let ((order '()) (seen (make-hash-table :test 'equal)))
    (labels ((visit (name version)
               (let ((key (repository-key name version)))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (let ((ns (gethash key repository)))
                     (when (gir-namespace-p ns)
                       (loop for (iname iversion) in (gir-namespace-includes ns)
                             do (visit iname iversion))
                       (when (member (list name version) targets :test #'equal)
                         (push ns order))))))))
      (loop for (name version) in targets do (visit name version)))
    (nreverse order)))

(defun make-context (repository targets)
  (let ((ctx (%make-context :repository repository
                            :targets (target-namespaces-in-order repository targets))))
    (dolist (ns (context-targets ctx))
      (let ((n (gir-namespace-name ns)))
        (dolist (item (append (gir-namespace-classes ns) (gir-namespace-enums ns)
                              (gir-namespace-callbacks ns) (gir-namespace-aliases ns)))
          (setf (gethash (qualify (gir-item-name item) n) (context-index ctx)) item))))
    ctx))

(defun lookup (ctx qualified-name)
  (gethash qualified-name (context-index ctx)))

(defun note-skip (ctx ns what reason)
  (push (cons what reason) (gethash (gir-namespace-name ns) (context-skipped ctx))))

;;; Phase 1: symbols

(defvar *item-symbols* (make-hash-table :test 'eq)
  "Model item (callable, constant, property) -> its Lisp symbol.")

(defun item-symbol (item) (gethash item *item-symbols*))

(defun recreate-package (name)
  "A fresh package NAME using CL. Only safe in an image that has not loaded
the generated bindings, which is why the generator runs in its own process."
  (let ((old (find-package name)))
    (when old (delete-package old)))
  (let ((package (make-package name :use '("COMMON-LISP"))))
    (sb-ext:add-package-local-nickname "RT" "GTK4.RUNTIME" package)
    package))

(defun claim-symbol (ctx package name description &optional (space :function))
  "Intern and export NAME in PACKAGE, shadowing a CL symbol of that name.
SPACE is the Lisp namespace the item uses (:type, :function or :variable);
a type and a function may share a symbol. Returns the symbol, or NIL if
another item already owns the name in that SPACE."
  (let ((key (format nil "~a:~a:~a" (package-name package) name space)))
    (if (gethash key (context-owners ctx))
        nil
        (progn
          (setf (gethash key (context-owners ctx)) description)
          (when (cl-symbol-p name) (shadow (string-upcase name) package))
          (let ((sym (intern (string-upcase name) package)))
            (export sym package)
            sym)))))

(defun callable-lisp-name (owner-kebab callable)
  (let ((name (or (gir-callable-shadows callable) (gir-item-name callable))))
    (if owner-kebab
        (format nil "~a-~a" owner-kebab (snake-to-kebab name))
        (snake-to-kebab name))))

(defun skip-callable-p (callable)
  "A reason CALLABLE is never bound, or NIL."
  (cond ((not (gir-item-introspectable callable)) "not introspectable")
        ((gir-callable-shadowed-by callable) "shadowed by another function")
        ((gir-callable-moved-to callable) "moved elsewhere (alias)")))

(defun name-namespaces (ctx)
  "Create a package per target namespace and claim a symbol for every item.
Symbols are stored on the model items under the :LISP-SYMBOL property list
of the hash table *ITEM-SYMBOLS*."
  (clrhash *type-symbols*)
  (clrhash *item-symbols*)
  (dolist (ns (context-targets ctx))
    (let* ((nsname (gir-namespace-name ns))
           (package (recreate-package (namespace-package-name nsname))))
      ;; Types first, so they win name collisions.
      (dolist (item (append (gir-namespace-enums ns) (gir-namespace-classes ns)
                            (gir-namespace-callbacks ns) (gir-namespace-aliases ns)))
        (let* ((qualified (qualify (gir-item-name item) nsname))
               (runtime (cdr (assoc qualified *runtime-types* :test #'string=)))
               (sym (if runtime
                        (let ((s (find-symbol runtime "GTK4.RUNTIME")))
                          (import s package)
                          (export s package)
                          s)
                        (claim-symbol ctx package (camel-to-kebab (gir-item-name item)) qualified :type))))
          (setf (gethash qualified *type-symbols*) sym)))
      (dolist (c (gir-namespace-constants ns))
        (setf (gethash c *item-symbols*)
              (claim-symbol ctx package (constant-lisp-name (gir-item-name c))
                            (qualify (gir-item-name c) nsname) :variable)))
      (flet ((name-callables (owner-kebab callables)
               (dolist (f callables)
                 (unless (skip-callable-p f)
                   (let ((sym (claim-symbol ctx package (callable-lisp-name owner-kebab f)
                                            (or (gir-callable-c-identifier f) (gir-item-name f)))))
                     (when sym
                       (setf (gethash f *item-symbols*) sym)))))))
        (name-callables nil (gir-namespace-functions ns))
        (dolist (c (gir-namespace-classes ns))
          (let ((kebab (camel-to-kebab (gir-item-name c))))
            (name-callables kebab (gir-class-constructors c))
            (name-callables kebab (gir-class-functions c))
            (name-callables kebab (gir-class-methods c))))
        (dolist (e (gir-namespace-enums ns))
          (name-callables (camel-to-kebab (gir-item-name e)) (gir-enum-functions e))))
      ;; Property accessors last; they lose collisions with methods.
      (dolist (c (gir-namespace-classes ns))
        (when (member (gir-class-kind c) '(:class :interface))
          (dolist (p (gir-class-properties c))
            (let ((sym (claim-symbol ctx package
                                     (format nil "~a-~a" (camel-to-kebab (gir-item-name c))
                                             (gir-item-name p))
                                     (format nil "~a:~a" (gir-item-name c) (gir-item-name p)))))
              (if sym
                  (setf (gethash p *item-symbols*) sym)
                  (note-skip ctx ns (format nil "~a:~a" (gir-item-name c) (gir-item-name p))
                             "property accessor name collision")))))))))

;;; Classifying types

(defparameter *basic-types*
  '(("gboolean" . :boolean) ("gchar" . :int8) ("guchar" . :uint8)
    ("gint8" . :int8) ("guint8" . :uint8) ("gint16" . :int16) ("guint16" . :uint16)
    ("gint32" . :int32) ("guint32" . :uint32) ("gint64" . :int64) ("guint64" . :uint64)
    ("gshort" . :short) ("gushort" . :ushort) ("gint" . :int) ("guint" . :uint)
    ("glong" . :long) ("gulong" . :ulong) ("gsize" . :size) ("gssize" . :ssize)
    ("gintptr" . :intptr) ("guintptr" . :uintptr) ("gfloat" . :float) ("gdouble" . :double)
    ("gunichar" . :uint32) ("gunichar2" . :uint16) ("goffset" . :int64)
    ("time_t" . :long) ("off_t" . :int64) ("pid_t" . :int) ("uid_t" . :uint)
    ("GType" . :gtype) ("gpointer" . :pointer) ("gconstpointer" . :pointer)
    ("none" . :void) ("utf8" . :string) ("filename" . :string)))

(defparameter *pointer-records* '("GLib.Variant" "GLib.VariantType")
  "Records with a GType that is not boxed (fundamental or special); passed as
raw pointers until they get dedicated wrappers.")

(defun namespace-of (qualified)
  (subseq qualified 0 (position #\. qualified)))

(defun gobject-type-p (ctx qualified)
  "True when QUALIFIED names a class descending from GObject, or an interface."
  (let ((name qualified))
    (loop repeat 64
          do (when (string= name "GObject.Object") (return t))
             (let ((item (lookup ctx name)))
               (cond ((not (gir-class-p item)) (return nil))
                     ((eq (gir-class-kind item) :interface) (return t))
                     ((not (eq (gir-class-kind item) :class)) (return nil))
                     ((gir-class-fundamental item) (return nil))
                     ((null (gir-class-parent item)) (return nil))
                     (t (setf name (qualify (gir-class-parent item) (namespace-of name)))))))))

(defun classify-type (ctx type nsname)
  "The DEFINE-GFUNCTION spec for TYPE, or (VALUES NIL reason)."
  (typecase type
    (null (values nil "missing type"))
    ((eql :varargs) (values nil "varargs"))
    (gir-array (classify-array ctx type nsname))
    (gir-callable (values nil "inline callback"))
    (gir-type
     (let* ((name (gir-type-name type))
            (basic (cdr (assoc name *basic-types* :test #'string=))))
       (cond
         (basic basic)
         ((null name) (values nil "untyped"))
         ((member name '("GLib.List" "GLib.SList" "GLib.HashTable" "GLib.Array"
                         "GLib.PtrArray" "GLib.ByteArray")
                  :test #'string=)
          (values nil (format nil "container ~a" name)))
         (t
          (let* ((qualified (qualify name nsname))
                 (item (lookup ctx qualified))
                 (sym (type-symbol qualified)))
            (cond
              ((member qualified *pointer-records* :test #'string=) :pointer)
              ((string= qualified "GObject.Object") (list :object sym))
              ((null item) (values nil (format nil "unresolved type ~a" qualified)))
              ((gir-enum-p item)
               (list (if (eq (gir-enum-kind item) :bitfield) :flags :enum) sym))
              ((gir-alias-p item)
               (classify-type ctx (gir-alias-type item) (namespace-of qualified)))
              ((gir-callable-p item)
               (let ((cb (callback-plan ctx qualified)))
                 (if (stringp cb)
                     (values nil (format nil "unsupported callback type: ~a" cb))
                     (list :callback sym))))
              ((member (gir-class-kind item) '(:class :interface))
               (if (gobject-type-p ctx qualified)
                   (list :object sym)
                   :pointer))
              ((and (gir-class-get-type item) (not (gir-class-is-gtype-struct-for item)))
               (list :boxed (gir-class-glib-type-name item) (gir-class-get-type item)))
              (t (list :record sym))))))))
    (t (values nil "unknown type form"))))

;;; Arrays

(defparameter *array-element-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype :pointer
    :string :object :enum :flags)
  "Element specs a C array may hold. Structs stored inline are not supported yet.")

(defun classify-array (ctx type nsname)
  "The spec for a GIR <array>: :strv, :byte-array, or
(:array ELEMENT [:length-index I] [:zero-terminated t] [:fixed-size N])."
  (let* ((name (gir-array-name type))
         (length (gir-array-length type))
         (fixed (gir-array-fixed-size type))
         (zt (case (gir-array-zero-terminated type)
               ((t) t)
               ((nil) nil)
               (t (and (null length) (null fixed)))))
         (element (gir-array-element type)))
    (cond
      ((equal name "GLib.ByteArray") :byte-array)
      (name (values nil (format nil "container ~a" name)))
      ((not (gir-type-p element)) (values nil "nested array"))
      ((and zt (null length) (null fixed)
            (member (gir-type-name element) '("utf8" "filename") :test #'equal))
       :strv)
      ((not (or length fixed zt)) (values nil "array without length"))
      (t
       (multiple-value-bind (spec why) (classify-type ctx element nsname)
         (cond
           ((null spec) (values nil (format nil "array element: ~a" why)))
           ((and (member (spec-kind* spec) '(:boxed :record))
                 (search "**" (or (gir-array-c-type type) "")))
            ;; An array of pointers to structs.
            (list* :array :pointer (array-options length zt fixed)))
           ((not (member (spec-kind* spec) *array-element-kinds*))
            (values nil (format nil "array of ~(~a~)" (spec-kind* spec))))
           (t (list* :array spec (array-options length zt fixed)))))))))

(defun array-options (length zt fixed)
  (append (when length (list :length-index length))
          (when zt (list :zero-terminated t))
          (when fixed (list :fixed-size fixed))))

(defun strip-length-index (spec)
  (if (and (consp spec) (eq (first spec) :array))
      (list* :array (second spec)
             (loop for (k v) on (cddr spec) by #'cddr
                   unless (eq k :length-index) append (list k v)))
      spec))

;;; Planning callables

(defun instance-spec (ctx owner nsname)
  "The spec for a method's instance parameter on OWNER (a gir-class)."
  (let ((qualified (qualify (gir-item-name owner) nsname)))
    (cond ((member (gir-class-kind owner) '(:class :interface))
           (if (gobject-type-p ctx qualified)
               (list :object (type-symbol qualified))
               :pointer))
          ((member qualified *pointer-records* :test #'string=) :pointer)
          ((gir-class-get-type owner)
           (list :boxed (gir-class-glib-type-name owner) (gir-class-get-type owner)))
          (t (list :record (type-symbol qualified))))))

(defstruct plan
  symbol c-name args return return-transfer throws version deprecated doc kind owner source)

(defun param-variable (p package)
  (intern (string-upcase (safe-variable-name (or (gir-parameter-name p) "arg"))) package))

(defun callback-type-p (ctx type nsname)
  "True when TYPE names a GIR callback type."
  (and (gir-type-p type) (gir-type-name type)
       (gir-callable-p (lookup ctx (qualify (gir-type-name type) nsname)))))

;;; Callback types

(defun callback-user-data-index (params)
  "Index of a callback type's user_data parameter: the one annotated with
closure, else a trailing gpointer."
  (or (position-if #'gir-parameter-closure params)
      (let ((last (car (last params))))
        (and last (gir-type-p (gir-parameter-type last))
             (equal (gir-type-name (gir-parameter-type last)) "gpointer")
             (1- (length params))))))

(defun plan-callback-type (ctx qualified)
  "(ARGS RETURN RETURN-TRANSFER) for the callback type QUALIFIED, or a reason string."
  (let* ((cb (lookup ctx qualified))
         (nsname (namespace-of qualified))
         (package (symbol-package (type-symbol qualified)))
         (params (gir-callable-parameters cb))
         (data (callback-user-data-index params)))
    (block plan
      (unless data (return-from plan "no user_data parameter"))
      (let ((args
              (loop for p in params
                    for i from 0
                    collect (if (= i data)
                                (list (param-variable p package) :pointer :user-data t)
                                (progn
                                  (unless (eq (gir-parameter-direction p) :in)
                                    (return-from plan "out or inout argument"))
                                  (when (callback-type-p ctx (gir-parameter-type p) nsname)
                                    (return-from plan "callback argument"))
                                  (multiple-value-bind (spec why)
                                      (classify-type ctx (gir-parameter-type p) nsname)
                                    (unless spec (return-from plan why))
                                    (when (member (spec-kind* spec) '(:array :byte-array))
                                      (return-from plan "array argument"))
                                    (list (param-variable p package) spec
                                          :transfer (or (gir-parameter-transfer p) :none)))))))
            (ret (multiple-value-list
                  (classify-type ctx (gir-callable-return-type cb) nsname))))
        (unless (first ret) (return-from plan (format nil "return: ~a" (second ret))))
        (when (member (spec-kind* (first ret)) '(:array :byte-array))
          (return-from plan "returns an array"))
        (when (and (eq (spec-kind* (first ret)) :string)
                   (not (eq (gir-callable-return-transfer cb) :full)))
          (return-from plan "returns a borrowed string"))
        (list args (first ret) (or (gir-callable-return-transfer cb) :none))))))

(defun spec-kind* (spec) (if (consp spec) (first spec) spec))

(defun callback-plan (ctx qualified)
  (multiple-value-bind (plan found) (gethash qualified (context-callback-plans ctx))
    (if found
        plan
        (progn
          ;; Mark in progress so a callback type mentioning itself cannot loop.
          (setf (gethash qualified (context-callback-plans ctx)) "recursive callback type")
          (setf (gethash qualified (context-callback-plans ctx))
                (plan-callback-type ctx qualified))))))

;;; Functions

(defun hidden-parameters (ctx params nsname)
  "A hash table: parameter -> (:user-data-of CALLBACK-PARAM) or
(:destroy-of CALLBACK-PARAM), for the user_data and GDestroyNotify
parameters that belong to callback parameters. Indexes in GIR count
parameters after the instance parameter."
  (let* ((plain (remove-if #'gir-parameter-instance-p params))
         (hidden (make-hash-table :test 'eq)))
    (dolist (p plain)
      (when (gir-parameter-destroy p)
        (let ((d (nth (gir-parameter-destroy p) plain)))
          (when d (setf (gethash d hidden) (list :destroy-of p))))))
    (dolist (p plain)
      (when (and (not (gethash p hidden))
                 (callback-type-p ctx (gir-parameter-type p) nsname)
                 (gir-parameter-closure p))
        (let ((u (nth (gir-parameter-closure p) plain)))
          (when (and u (not (eq u p)))
            (setf (gethash u hidden) (list :user-data-of p))))))
    ;; Older annotation style: closure on the user_data parameter, pointing
    ;; at its callback.
    (dolist (p plain)
      (when (and (not (gethash p hidden))
                 (not (callback-type-p ctx (gir-parameter-type p) nsname))
                 (gir-parameter-closure p))
        (let ((c (nth (gir-parameter-closure p) plain)))
          (when (and c (callback-type-p ctx (gir-parameter-type c) nsname)
                     (not (gir-parameter-closure c)))
            (setf (gethash p hidden) (list :user-data-of c))))))
    hidden))

(defun plan-callable (ctx ns callable owner)
  "A PLAN for CALLABLE (whose method owner is OWNER, or NIL), or (VALUES NIL reason)."
  (let ((nsname (gir-namespace-name ns)))
    (block plan
      (flet ((fail (reason) (return-from plan (values nil reason))))
        (let ((reason (skip-callable-p callable)))
          (when reason (fail reason)))
        (unless (item-symbol callable) (fail "name collision"))
        (let* ((params (gir-callable-parameters callable))
               (package (symbol-package (item-symbol callable)))
               (hidden (hidden-parameters ctx params nsname))
               (vars (loop for p in params collect (cons p (param-variable p package))))
               (callbacks-with-data
                 (loop for v being the hash-values of hidden
                       when (eq (first v) :user-data-of) collect (second v)))
               (plain (remove-if #'gir-parameter-instance-p params))
               ;; length parameter -> the array parameter it measures, or :return
               (lengths (make-hash-table :test 'eq))
               (args '()))
          (flet ((note-length (array-type target)
                   (when (and (gir-array-p array-type) (gir-array-length array-type))
                     (let ((len (nth (gir-array-length array-type) plain)))
                       (cond ((null len) (fail "array length index out of range"))
                             ((gethash len lengths) (fail "shared array length parameter"))
                             (t (setf (gethash len lengths) target)))))))
            (dolist (p plain) (note-length (gir-parameter-type p) p))
            (note-length (gir-callable-return-type callable) :return))
          (dolist (p params)
            (let ((direction (gir-parameter-direction p))
                  (role (gethash p hidden)))
              (cond
                (role
                 (push (list (cdr (assoc p vars)) :pointer
                             (first role) (cdr (assoc (second role) vars)))
                       args))
                ;; An array's length parameter, filled in or read automatically.
                ((and (gethash p lengths)
                      (not (let ((target (gethash p lengths)))
                             ;; A caller-allocated out array whose size the caller passes.
                             (and (gir-parameter-p target)
                                  (eq (gir-parameter-direction target) :out)
                                  (gir-parameter-caller-allocates target)
                                  (eq direction :in)))))
                 (let* ((target (gethash p lengths))
                        (target-direction (if (eq target :return) :out (gir-parameter-direction target))))
                   (unless (eq direction target-direction)
                     (fail "array length direction mismatch"))
                   (multiple-value-bind (spec why) (classify-type ctx (gir-parameter-type p) nsname)
                     (unless spec (fail why))
                     (push (list (cdr (assoc p vars)) spec
                                 :direction direction
                                 :length-of (if (eq target :return) :return (cdr (assoc target vars))))
                           args))))
                (t
                 (when (eq direction :inout) (fail "inout parameter"))
                 (when (and (eq direction :out) (gir-parameter-caller-allocates p)
                            (not (gir-array-p (gir-parameter-type p))))
                   (fail "caller-allocates out parameter"))
                 (multiple-value-bind (spec why)
                     (if (gir-parameter-instance-p p)
                         (instance-spec ctx owner nsname)
                         (classify-type ctx (gir-parameter-type p) nsname))
                   (unless spec (fail why))
                   (when (eq spec :void) (fail "void parameter"))
                   (when (eq (spec-kind* spec) :callback)
                     (unless (member p callbacks-with-data)
                       (fail "callback without user_data"))
                     (setf spec (list :callback (second spec)
                                      (or (gir-parameter-scope p) :call))))
                   (when (and (gir-parameter-scope p) (not (eq (spec-kind* spec) :callback)))
                     (fail "scope on a non-callback parameter"))
                   (when (eq (spec-kind* spec) :array)
                     (when (and (eq direction :in)
                                (member (gir-parameter-transfer p) '(:full :container)))
                       (fail "array argument with ownership transfer"))
                     (let ((len (and (gir-array-length (gir-parameter-type p))
                                     (nth (gir-array-length (gir-parameter-type p)) plain))))
                       (setf spec (strip-length-index spec))
                       (when (and (eq direction :out) (gir-parameter-caller-allocates p))
                         (when (and (null len) (null (getf (cddr spec) :fixed-size)))
                           (fail "caller-allocated array without a size"))
                         (setf spec (append spec
                                            (when (and len (eq (gir-parameter-direction len) :in))
                                              (list :length (cdr (assoc len vars))))
                                            '(:caller-allocates t))))))
                   (push (list (cdr (assoc p vars))
                               spec
                               :direction direction
                               :transfer (or (gir-parameter-transfer p) :none)
                               :nullable (gir-parameter-nullable p))
                         args))))))
          (setf args (nreverse args))
          ;; Trailing nullable :in arguments become &optional (hidden ones are skipped).
          (loop for a in (reverse args)
                do (cond ((or (getf (cddr a) :user-data-of) (getf (cddr a) :destroy-of)))
                         ((and (eq (getf (cddr a) :direction) :in)
                               (getf (cddr a) :nullable)
                               (not (eq a (first args))))
                          (setf (getf (cddr a) :optional) t))
                         (t (return))))
          (multiple-value-bind (ret why)
              (classify-type ctx (gir-callable-return-type callable) nsname)
            (unless ret (fail (format nil "return: ~a" why)))
            (when (eq (spec-kind* ret) :callback) (fail "returns a callback"))
            (setf ret (strip-length-index ret))
            (make-plan :symbol (item-symbol callable)
                       :c-name (gir-callable-c-identifier callable)
                       :args args
                       :return ret
                       :return-transfer (or (gir-callable-return-transfer callable) :none)
                       :throws (gir-callable-throws callable)
                       :version (gir-item-version callable)
                       :deprecated (gir-item-deprecated callable)
                       :doc (gir-item-doc callable)
                       :kind (gir-callable-kind callable)
                       :owner owner
                       :source callable)))))))
