;;;; subclass.lisp — GTypes defined in Lisp, and virtual function overrides
;;;;
;;;; A class with metaclass GOBJECT-CLASS that names a :gtype-name but no
;;;; :get-type is defined in Lisp: the first time its GType is needed, it is
;;;; registered with GLib as a subclass of the nearest GType class in its
;;;; precedence list. Interface classes among its superclasses are added to
;;;; the GType too.
;;;;
;;;; Virtual functions: the generated bindings describe every overridable
;;;; virtual function (DEFINE-GVFUNC): which class struct slot holds it, a
;;;; trampoline that enters Lisp from C, and a caller that calls a C
;;;; implementation from Lisp. DEFINE-VFUNC stores a Lisp implementation on a
;;;; class; the class's struct slot points at the trampoline, which finds the
;;;; implementation for the instance's class at each call. Redefining an
;;;; implementation therefore takes effect immediately.

(in-package #:gtk4.runtime)

;;; GLib's type system

(cffi:defcstruct gtype-query
  (type gtype) (type-name :pointer) (class-size :uint) (instance-size :uint))

(cffi:defcstruct ginterface-info
  (interface-init :pointer) (interface-finalize :pointer) (interface-data :pointer))

(cffi:defcfun ("g_type_query" %g-type-query) :void (type gtype) (query :pointer))
(cffi:defcfun ("g_type_register_static_simple" %g-type-register-static-simple) gtype
  (parent gtype) (name :string) (class-size :uint) (class-init :pointer)
  (instance-size :uint) (instance-init :pointer) (flags :int))
(cffi:defcfun ("g_type_add_interface_static" %g-type-add-interface-static) :void
  (type gtype) (interface gtype) (info :pointer))
(cffi:defcfun ("g_type_class_peek" %g-type-class-peek) :pointer (type gtype))
(cffi:defcfun ("g_type_interface_peek" %g-type-interface-peek) :pointer
  (class :pointer) (interface gtype))

(defconstant +g-type-flag-abstract+ (ash 1 4))

(defun interface-gtype-p (gtype)
  (= (gtype-fundamental gtype) +g-type-interface+))

(defun class-struct-gtype (class-struct)
  "G_TYPE_FROM_CLASS."
  (cffi:foreign-slot-value class-struct '(:struct gtype-class) 'g-type))

;;; Virtual function descriptions (from the generated bindings)

(defstruct (vfunc-info (:constructor %make-vfunc-info))
  owner           ; symbol naming the class or interface that declares it
  name            ; keyword
  struct          ; class-struct layout symbol
  slot            ; slot holding the function pointer
  handler         ; (lambda (raw-arg...)) run when C calls the function
  signature       ; (return-type (arg-type...)) as CFFI types
  (trampoline nil); foreign pointer to the C entry point, made on first use
  caller          ; (lambda (function-pointer &rest args)) calling a C implementation
  lambda-list     ; argument names, for documentation and describe
  url             ; upstream documentation
  (offset nil))   ; byte offset of SLOT, computed on first use

(defvar *vfuncs* (make-hash-table :test 'equal)
  "(OWNER . NAME) -> VFUNC-INFO.")

(defun register-vfunc (owner name struct slot handler signature caller lambda-list url)
  (let ((old (gethash (cons owner name) *vfuncs*)))
    (if old
        ;; Reloading the bindings: keep the entry point (and implementations,
        ;; which are keyed by this object).
        (setf (vfunc-info-struct old) struct (vfunc-info-slot old) slot
              (vfunc-info-handler old) handler (vfunc-info-signature old) signature
              (vfunc-info-caller old) caller (vfunc-info-lambda-list old) lambda-list
              (vfunc-info-url old) url (vfunc-info-offset old) nil)
        (setf (gethash (cons owner name) *vfuncs*)
              (%make-vfunc-info :owner owner :name name :struct struct :slot slot
                                :handler handler :signature signature :caller caller
                                :lambda-list lambda-list :url url)))))

(defun vfunc-offset (info)
  (or (vfunc-info-offset info)
      (setf (vfunc-info-offset info)
            (cffi:foreign-slot-offset (struct-type (vfunc-info-struct info)) (vfunc-info-slot info)))))

(defun vfunc-owner-interface-p (info)
  (interface-gtype-p (class-gtype (vfunc-info-owner info))))

(defun vfunc-table (class-struct info)
  "Where INFO's function pointer lives for the class whose struct is
CLASS-STRUCT: the class struct itself, or its vtable for INFO's interface.
NIL if the class does not implement that interface."
  (if (vfunc-owner-interface-p info)
      (let ((iface (%g-type-interface-peek class-struct (class-gtype (vfunc-info-owner info)))))
        (and (not (cffi:null-pointer-p iface)) iface))
      class-struct))

(defun vfunc-pointer (table info)
  (cffi:mem-ref table :pointer (vfunc-offset info)))

(defun (setf vfunc-pointer) (value table info)
  (setf (cffi:mem-ref table :pointer (vfunc-offset info)) value))

(defun find-vfunc (class name &optional owner)
  "The VFUNC-INFO for virtual function NAME (a keyword) as seen from CLASS:
declared by OWNER if given, else by the nearest class or interface in
CLASS's precedence list that declares one by that name."
  (let ((class (if (symbolp class) (find-class class) class)))
    (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
    (if owner
        (or (gethash (cons owner name) *vfuncs*)
            (error "gtk4: ~s has no virtual function ~s" owner name))
        (or (loop for c in (sb-mop:class-precedence-list class)
                  for info = (gethash (cons (class-name c) name) *vfuncs*)
                  when info return info)
            (error "gtk4: no class or interface ~s inherits from has a virtual function ~s~@[; ~
                    did you mean ~s?~]"
                   (class-name class) name
                   (loop for (o . n) being the hash-keys of *vfuncs*
                         when (and (member o (mapcar #'class-name (sb-mop:class-precedence-list class)))
                                   (search (string name) (string n)))
                           return n))))))

;;; Generated: one DEFINE-GVFUNC per overridable virtual function

(defun vfunc-trampoline-name (owner name)
  (intern (format nil "VFUNC ~a:~a ~a" (package-name (symbol-package owner)) (symbol-name owner)
                  (symbol-name name))
          '#:gtk4.runtime))

(defmacro define-gvfunc ((owner name) (struct slot) &key args (return :void) (return-transfer :none)
                                                         throws url documentation)
  "Describe virtual function NAME of OWNER, stored in SLOT of class struct
STRUCT. ARGS are as for DEFINE-GFUNCTION; the first is the instance. Defines
the handler that converts C arguments and runs the Lisp implementation, and
the caller that chains to a C implementation."
  (declare (ignore documentation))
  (let* ((ins (remove :out args :key (lambda (a) (getf (cddr a) :direction :in))))
         (outs (remove :in args :key (lambda (a) (getf (cddr a) :direction :in))))
         (result (gensym "RESULT"))
         (out-values (loop for a in outs collect (gensym (string (first a)))))
         (fn (gensym "FN"))
         (err (gensym "ERROR")))
    (multiple-value-bind (required optional body)
        (gfunction-parts args return return-transfer throws fn)
      (declare (ignore optional))
      `(register-vfunc
        ',owner ',name ',struct ,slot
        ;; The handler takes the raw C arguments. Its C entry point is made
        ;; only when a class first overrides this function (see ENSURE-TRAMPOLINE):
        ;; SBCL has room for a limited number of them.
        (lambda (,@(mapcar #'first args) ,@(when throws (list err)))
          (,@(if throws
                 ;; A Lisp error becomes the GError C expects.
                 `(handler-case-gerror (,err (list ',owner ',name) ,(foreign-zero return)))
                 `(with-callback-protection ((list ',owner ',name) ,(foreign-zero return))))
            (multiple-value-call
                (lambda (&optional ,@(unless (eq return :void) (list result)) ,@out-values)
                  ,@(loop for (var spec . options) in outs
                          for v in out-values
                          collect `(unless (cffi:null-pointer-p ,var)
                                     (setf (cffi:mem-ref ,var ',(spec-foreign-type spec))
                                           ,(vfunc-return-form v spec (getf options :transfer :none)))))
                  ,(unless (eq return :void)
                     (vfunc-return-form result return return-transfer)))
              (invoke-vfunc ',owner ',name
                            ,@(loop for (var spec . options) in ins
                                    collect (convert-from-foreign var spec
                                                                  (getf options :transfer :none)))))))
        '(,(spec-foreign-type return)
          ,(append (loop for (nil spec . options) in args
                         collect (if (eq (getf options :direction :in) :out) :pointer (spec-foreign-type spec)))
                   (when throws '(:pointer))))
        (lambda (,fn ,@required) (with-gtk-float-traps ,body))
        ',required ,url))))

(defmacro handler-case-gerror ((error-location where default) &body body)
  "Like WITH-CALLBACK-PROTECTION, but a failure is also stored in the GError**
ERROR-LOCATION, so C sees the error it expects alongside DEFAULT."
  `(with-gtk-float-traps
     (handler-case (progn ,@body)
       (error (e)
         (unless (typep e 'glib-error)
           (funcall *callback-error-handler* e ,where))
         (set-gerror-from-condition ,error-location e)
         ,default))))

(defun ensure-trampoline (info)
  "The C entry point for INFO's handler, made on first use."
  (or (vfunc-info-trampoline info)
      (setf (vfunc-info-trampoline info)
            (destructuring-bind (return arg-types) (vfunc-info-signature info)
              (let ((name (vfunc-trampoline-name (vfunc-info-owner info) (vfunc-info-name info)))
                    (vars (loop for i below (length arg-types) collect (gensym "ARG"))))
                (eval `(progn
                         (cffi:defcallback ,name ,return ,(mapcar #'list vars arg-types)
                           (funcall (vfunc-info-handler ,info) ,@vars))
                         (cffi:callback ,name))))))))

(defun vfunc-return-form (form spec transfer)
  "Convert a Lisp implementation's value FORM for C. NIL is the zero value
of every type, so an implementation that returns nothing is harmless."
  (case (spec-kind spec)
    ((:enum :flags) `(let ((v ,form)) (if v ,(convert-to-foreign 'v spec transfer) 0)))
    (t (convert-to-foreign form spec transfer))))

;;; Lisp implementations

(defun lisp-gtype-class-p (class)
  "True for a class whose GType is registered by Lisp."
  (and (typep class 'gobject-class) (class-lisp-defined-p class)))

(defun c-gtype-class-p (class)
  "True for a binding of a C class (not an interface)."
  (and (typep class 'gobject-class)
       (class-gtype-name class)
       (not (class-lisp-defined-p class))
       (not (interface-gtype-p (class-gtype class)))))

(defun vfunc-implementation (class info)
  (and (typep class 'gobject-class)
       (gethash info (class-vfunc-implementations class))))

(defun invoke-vfunc (owner name self &rest args)
  "Called by a trampoline: run the implementation of OWNER's virtual
function NAME for SELF's class."
  (let ((info (gethash (cons owner name) *vfuncs*)))
    (call-vfunc-chain (sb-mop:class-precedence-list (class-of self)) info (cons self args))))

(defun call-vfunc-chain (classes info args)
  "Call the first implementation of INFO found in CLASSES: a Lisp one, or
else the C one of the first C class. With neither, return no values."
  (loop for tail on classes
        for class = (first tail)
        for impl = (vfunc-implementation class info)
        do (cond
             (impl
              (return (apply impl
                             (let ((rest (rest tail)))
                               (lambda (&rest next-args)
                                 (call-vfunc-chain rest info (or next-args args))))
                             args)))
             ((c-gtype-class-p class)
              (return (call-c-vfunc class info args))))
        finally (return (values))))

(defun call-c-vfunc (class info args)
  "Call CLASS's C implementation of INFO, if it has one."
  (let* ((struct (%g-type-class-peek (class-gtype class)))
         (table (and (not (cffi:null-pointer-p struct)) (vfunc-table struct info)))
         (fn (and table (vfunc-pointer table info))))
    (if (and fn (not (cffi:null-pointer-p fn)))
        (apply (vfunc-info-caller info) fn args)
        (values))))

(defmacro define-vfunc ((class name &optional owner) lambda-list &body body)
  "Implement virtual function NAME (a keyword such as :snapshot) for CLASS,
a Lisp-defined GObject class. LAMBDA-LIST names the instance and the
function's other arguments, in the C order (out arguments are returned as
extra values instead). OWNER names the declaring class or interface when
NAME alone is ambiguous.

Within BODY, (call-next-vfunc) calls the next implementation, Lisp or C,
with the same arguments; (call-next-vfunc arg...) passes ARGs instead,
starting with the instance. Redefining takes effect at once, including for
existing instances."
  (multiple-value-bind (forms declarations doc) (alexandria:parse-body body :documentation t)
    (declare (ignore doc))
    (let* ((next (gensym "NEXT"))
           (args (gensym "ARGS"))
           ;; CALL-NEXT-VFUNC works qualified (gobject:call-next-vfunc) or not.
           (names (remove-duplicates (list 'call-next-vfunc (intern "CALL-NEXT-VFUNC" *package*)))))
      `(add-vfunc-implementation
        ',class ',name ',owner
        (lambda (,next &rest ,args)
          (flet ,(loop for n in names
                       collect `(,n (&rest next-args) (apply ,next (or next-args ,args))))
            (declare (ignorable ,@(loop for n in names collect `(function ,n))))
            (apply (lambda ,lambda-list ,@declarations ,@forms) ,args)))))))

(defun add-vfunc-implementation (class-name name owner function)
  (let* ((class (find-class class-name))
         (info (find-vfunc class name owner)))
    (unless (lisp-gtype-class-p class)
      (error "gtk4: ~s cannot override virtual functions: give it its own ~
              (:gtype-name \"...\") so it is registered as a new GType" class-name))
    (when (and (eq (vfunc-info-owner info) 'object) (eq name :finalize))
      (error "gtk4: :finalize cannot be implemented in Lisp (the instance is already ~
              unreachable); use :dispose instead"))
    (setf (gethash info (class-vfunc-implementations class)) function)
    ;; Classes registered already: point their slots at the trampoline now.
    (let ((gtype (slot-value class 'gtype)))
      (when gtype
        (dolist (g (cons gtype (lisp-subtypes gtype)))
          (let ((struct (%g-type-class-peek g)))
            (unless (cffi:null-pointer-p struct)
              (let ((table (vfunc-table struct info)))
                (when table
                  (setf (vfunc-pointer table info) (ensure-trampoline info)))))))))
    (list class-name name)))

(defun remove-vfunc (class-name name &optional owner)
  "Remove CLASS-NAME's Lisp implementation of NAME. Calls then chain to the
next implementation."
  (let ((class (find-class class-name)))
    (remhash (find-vfunc class name owner) (class-vfunc-implementations class))))

;;; Registering Lisp-defined GTypes

(defvar *lisp-gtypes* (make-hash-table)
  "GType -> the Lisp class that registered it.")

(defun lisp-subtypes (gtype)
  "Registered Lisp GTypes descending from GTYPE."
  (loop for g being the hash-keys of *lisp-gtypes*
        when (and (/= g gtype) (gtype-is-a g gtype)) collect g))

(defun parent-gtype-class (class)
  "The class whose GType CLASS's GType derives from."
  (or (find-if (lambda (c)
                 (and (not (eq c class)) (typep c 'gobject-class) (class-gtype-name c)
                      (not (interface-gtype-p (class-gtype c)))))
               (sb-mop:class-precedence-list class))
      (error "gtk4: ~s has no GObject superclass" (class-name class))))

(defun implements-for-interface-p (class interface)
  "True when CLASS or a Lisp superclass implements a virtual function of INTERFACE."
  (some (lambda (c)
          (and (typep c 'gobject-class)
               (loop for info being the hash-keys of (class-vfunc-implementations c)
                     thereis (eq (vfunc-info-owner info) (class-name interface)))))
        (sb-mop:class-precedence-list class)))

(defun interfaces-to-add (class parent-gtype)
  "Interface classes to add to CLASS's new GType, prerequisites first: those
PARENT-GTYPE does not implement, and those it does that CLASS lists as a
direct superclass or overrides (GObject then copies the parent's
implementations, and the overrides replace some of them)."
  (reverse
   (remove-if-not (lambda (c)
                    (and (typep c 'gobject-class) (class-gtype-name c)
                         (not (class-lisp-defined-p c))
                         (interface-gtype-p (class-gtype c))
                         (or (not (gtype-is-a parent-gtype (class-gtype c)))
                             (member c (sb-mop:class-direct-superclasses class))
                             (implements-for-interface-p class c))))
                  (sb-mop:class-precedence-list class))))

(defun ensure-lisp-gtype (class)
  "The GType of Lisp-defined CLASS, registering it on first use. A type of
the same name registered earlier (before the class was redefined) is reused."
  (let* ((name (class-gtype-name class))
         (existing (gtype-from-name name)))
    (cond
      (existing
       (let ((previous (gethash existing *lisp-gtypes*)))
         (when (and previous (not (eq previous class)))
           (error "gtk4: GType ~s already belongs to ~s" name (class-name previous))))
       (setf (gethash existing *lisp-gtypes*) class)
       (check-class-redefinition class existing)
       existing)
      (t
       (let ((parent (class-gtype (parent-gtype-class class))))
         (cffi:with-foreign-object (q '(:struct gtype-query))
           (%g-type-query parent q)
           (let ((gtype (%g-type-register-static-simple
                         parent name
                         (cffi:foreign-slot-value q '(:struct gtype-query) 'class-size)
                         (cffi:callback lisp-class-init)
                         (cffi:foreign-slot-value q '(:struct gtype-query) 'instance-size)
                         (cffi:callback lisp-instance-init)
                         (if (class-abstract-p class) +g-type-flag-abstract+ 0))))
             (when (zerop gtype)
               (error "gtk4: GLib refused to register GType ~s" name))
             (setf (gethash gtype *lisp-gtypes*) class)
             (dolist (iface (interfaces-to-add class parent))
               (cffi:with-foreign-object (info '(:struct ginterface-info))
                 (setf (cffi:foreign-slot-value info '(:struct ginterface-info) 'interface-init)
                       (cffi:callback lisp-interface-init)
                       (cffi:foreign-slot-value info '(:struct ginterface-info) 'interface-finalize)
                       (cffi:null-pointer)
                       (cffi:foreign-slot-value info '(:struct ginterface-info) 'interface-data)
                       (make-handle (cons class (class-name iface))))
                 (%g-type-add-interface-static gtype (class-gtype iface) info)))
             (setf (class-registered-shape class) (class-shape class))
             gtype)))))))

(defvar *class-init-hooks* '()
  "Functions called as (FUNCTION class class-struct) when a Lisp-defined
GType's class is initialized, after virtual functions are installed. The
GTK layer uses this to set up composite templates.")

(defvar *instance-init-hooks* '()
  "Functions called as (FUNCTION class instance-pointer) while a Lisp-defined
GType's instance is initialized.")

(cffi:defcallback lisp-class-init :void ((class-struct :pointer) (data :pointer))
  (declare (ignore data))
  (with-callback-protection ("class_init")
    (let ((class (gethash (class-struct-gtype class-struct) *lisp-gtypes*)))
      (install-vfunc-implementations class class-struct nil)
      (initialize-lisp-class class class-struct)
      (dolist (hook *class-init-hooks*) (funcall hook class class-struct)))))

(cffi:defcallback lisp-interface-init :void ((iface :pointer) (data :pointer))
  (with-callback-protection ("interface_init")
    (destructuring-bind (class . interface) (handle-value data)
      (install-vfunc-implementations class iface interface))))

(defun install-vfunc-implementations (class table interface)
  "Point TABLE's slots at the trampolines of the implementations CLASS and
its Lisp superclasses define: class virtual functions when INTERFACE is NIL,
else those of INTERFACE (a symbol)."
  (dolist (c (sb-mop:class-precedence-list class))
    (when (typep c 'gobject-class)
      (loop for info being the hash-keys of (class-vfunc-implementations c)
            when (if interface
                     (eq (vfunc-info-owner info) interface)
                     (not (vfunc-owner-interface-p info)))
              do (setf (vfunc-pointer table info) (ensure-trampoline info))))))

(cffi:defcallback lisp-instance-init :void ((instance :pointer) (class-struct :pointer))
  (declare (ignore class-struct))
  (with-callback-protection ("instance_init")
    ;; Tie the instance to its proxy before anything (a virtual function, a
    ;; property notification) can ask for one.
    ;; Lisp slots are initialized here too, so virtual functions called
    ;; during construction (:constructed, property setters) see them.
    (destructuring-bind (&optional proxy &rest slot-args) *constructing*
      (when (and proxy (not (slot-boundp proxy 'pointer)))
        (setf (slot-value proxy 'pointer) instance
              (gethash (cffi:pointer-address instance) *proxies*) proxy)
        (let ((*initializing-slots* t))
          (apply #'shared-initialize proxy t slot-args))))
    ;; While each type's instance_init runs, the instance reports that type.
    (let ((class (gethash (instance-gtype instance) *lisp-gtypes*)))
      (dolist (hook *instance-init-hooks*) (funcall hook class instance)))))

;;; Redefinition

(defun class-shape (class)
  "What a registered GType fixes at class_init: changing it needs a new GType."
  (list :properties (mapcar #'first (class-property-specs class))
        :signals (mapcar #'first (class-signal-specs class))
        :template (class-template class)))

(defun check-class-redefinition (class gtype)
  (let ((old (class-registered-shape class)))
    (when (and old (not (equal old (class-shape class))))
      (warn "gtk4: ~s changed its properties, signals or template, but GType ~a is ~
             already registered with the old ones. Slots and virtual functions update ~
             in place; for the rest, give the class a new :gtype-name (or restart)."
            (class-name class) (gtype-name gtype)))))
