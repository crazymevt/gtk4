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
  (bound (make-hash-table :test 'equal)))  ; namespace -> count of callables bound

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
    (gir-array
     (let ((element (gir-array-element type)))
       (if (and (null (gir-array-name type))
                (null (gir-array-length type))
                (null (gir-array-fixed-size type))
                (gir-type-p element)
                (member (gir-type-name element) '("utf8" "filename") :test #'string=))
           :strv
           (values nil "array"))))
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
              ((gir-callable-p item) (values nil "callback"))
              ((member (gir-class-kind item) '(:class :interface))
               (if (gobject-type-p ctx qualified)
                   (list :object sym)
                   :pointer))
              ((and (gir-class-get-type item) (not (gir-class-is-gtype-struct-for item)))
               (list :boxed (gir-class-glib-type-name item) (gir-class-get-type item)))
              (t (list :record sym))))))))
    (t (values nil "unknown type form"))))

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

(defun plan-callable (ctx ns callable owner)
  "A PLAN for CALLABLE (whose method owner is OWNER, or NIL), or (VALUES NIL reason)."
  (let ((nsname (gir-namespace-name ns)))
    (block plan
      (flet ((fail (reason) (return-from plan (values nil reason))))
        (let ((reason (skip-callable-p callable)))
          (when reason (fail reason)))
        (unless (item-symbol callable) (fail "name collision"))
        (let* ((params (gir-callable-parameters callable))
               (args '()))
          (dolist (p params)
            (let ((direction (gir-parameter-direction p)))
              (when (eq direction :inout) (fail "inout parameter"))
              (when (and (eq direction :out) (gir-parameter-caller-allocates p))
                (fail "caller-allocates out parameter"))
              (when (or (gir-parameter-closure p) (gir-parameter-destroy p)
                        (gir-parameter-scope p))
                (fail "callback parameter"))
              (multiple-value-bind (spec why)
                  (if (gir-parameter-instance-p p)
                      (instance-spec ctx owner nsname)
                      (classify-type ctx (gir-parameter-type p) nsname))
                (unless spec (fail why))
                (when (eq spec :void) (fail "void parameter"))
                (push (list (intern (string-upcase (safe-variable-name (or (gir-parameter-name p) "arg")))
                                    (symbol-package (item-symbol callable)))
                            spec
                            :direction direction
                            :transfer (or (gir-parameter-transfer p) :none)
                            :nullable (gir-parameter-nullable p))
                      args))))
          (setf args (nreverse args))
          ;; Trailing nullable :in arguments become &optional.
          (loop for a in (reverse args)
                while (and (eq (getf (cddr a) :direction) :in)
                           (getf (cddr a) :nullable)
                           (not (eq a (first args))))
                do (setf (getf (cddr a) :optional) t))
          (multiple-value-bind (ret why)
              (classify-type ctx (gir-callable-return-type callable) nsname)
            (unless ret (fail (format nil "return: ~a" why)))
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
