;;;; properties.lisp — GObject properties and signals defined in Lisp
;;;;
;;;; A slot of a Lisp-defined class becomes a GObject property with the
;;;; :property slot option:
;;;;
;;;;   (seconds :initform 0 :accessor clock-seconds
;;;;            :property (:int :min 0 :max 59))
;;;;
;;;; The value lives in the slot. GObject reads and writes it through the
;;;; class's get_property and set_property, so the property works with
;;;; GtkBuilder, g_object_bind_property and expressions; writing the slot from
;;;; Lisp (through its accessor or SETF SLOT-VALUE) checks the value against
;;;; the spec's type and :min/:max, as GObject does, and emits notify:: as C
;;;; code expects.
;;;;
;;;; A slot with :template-child "id" (or T, for an id equal to the slot's
;;;; name) reads the object with that id from the class's composite template,
;;;; the first time it is read.

(in-package #:gtk4.runtime)

;;; Slot definitions

(defclass gobject-direct-slot-definition (sb-mop:standard-direct-slot-definition)
  ((property :initarg :property :initform nil :reader slot-property)
   (template-child :initarg :template-child :initform nil :reader slot-template-child)))

(defclass gobject-effective-slot-definition (sb-mop:standard-effective-slot-definition)
  ((property :initform nil :accessor slot-property)
   (property-name :initform nil :accessor slot-property-name)
   (property-check :initform nil :accessor slot-property-check) ; (LISP-TYPE MIN MAX) or NIL
   (template-child :initform nil :accessor slot-template-child)
   (defining-class :initform nil :accessor slot-defining-class)))

(defmethod sb-mop:direct-slot-definition-class ((class gobject-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'gobject-direct-slot-definition))

(defvar *special-slot* nil
  "The direct slot definition with GObject options being made effective.")

(defmethod sb-mop:effective-slot-definition-class ((class gobject-class) &rest initargs)
  (declare (ignore initargs))
  ;; Only slots with GObject options get the special class, so ordinary
  ;; slots keep SBCL's fast accessors.
  (if *special-slot*
      (find-class 'gobject-effective-slot-definition)
      (call-next-method)))

(defmethod sb-mop:compute-effective-slot-definition ((class gobject-class) name direct-slots)
  (let* ((special (find-if (lambda (d)
                             (and (typep d 'gobject-direct-slot-definition)
                                  (or (slot-property d) (slot-template-child d))))
                           direct-slots))
         (effective (let ((*special-slot* special)) (call-next-method))))
    (when special
      (setf (slot-property effective) (slot-property special)
            (slot-property-name effective) (and (slot-property special)
                                                (string-downcase (symbol-name name)))
            (slot-property-check effective) (and (slot-property special)
                                                 (property-value-check (slot-property special)))
            (slot-template-child effective)
            (let ((id (slot-template-child special)))
              (if (eq id t) (string-downcase (symbol-name name)) id))
            (slot-defining-class effective)
            (find-if (lambda (c) (member special (sb-mop:class-direct-slots c)))
                     (sb-mop:class-precedence-list class))))
    effective))

(cffi:defcfun ("g_object_notify" %g-object-notify) :void (object :pointer) (name :string))

(defvar *setting-property* nil
  "True while GObject's set_property writes a slot; GObject notifies itself.")

;;; Writing a property slot from Lisp follows the rules GObject applies when
;;; C code sets the property: a numeric value must be a number of the right
;;; kind within the spec's :min and :max. (GObject validates through the
;;; param spec only on its own path, g_object_set_property and friends; a
;;; Lisp write goes straight to the slot.)

(define-condition property-value-error (error)
  ((object :initarg :object :reader property-value-error-object)
   (property :initarg :property :reader property-value-error-property)
   (value :initarg :value :reader property-value-error-value)
   (expected :initarg :expected :reader property-value-error-expected))
  (:report (lambda (c s)
             (format s "gtk4: ~s is not a valid value for property ~s of ~s: it must be ~a."
                     (property-value-error-value c) (property-value-error-property c)
                     (class-name (class-of (property-value-error-object c)))
                     (property-value-error-expected c)))))

(defparameter *numeric-property-ranges*
  `((:int integer ,(- (expt 2 31)) ,(1- (expt 2 31)))
    (:uint integer 0 ,(1- (expt 2 32)))
    (:long integer ,(- (expt 2 63)) ,(1- (expt 2 63)))
    (:ulong integer 0 ,(1- (expt 2 64)))
    (:int64 integer ,(- (expt 2 63)) ,(1- (expt 2 63)))
    (:uint64 integer 0 ,(1- (expt 2 64)))
    (:float real ,most-negative-single-float ,most-positive-single-float)
    (:double real ,most-negative-double-float ,most-positive-double-float))
  "(TYPE LISP-TYPE MIN MAX) for each numeric property type.")

(defun property-value-check (spec)
  "(LISP-TYPE MIN MAX) a value of a property with SPEC must satisfy, or NIL
for a type without a range."
  (destructuring-bind (type &key min max &allow-other-keys) (normalize-property-spec spec)
    (let ((range (and (keywordp type) (assoc type *numeric-property-ranges*))))
      (when range
        (destructuring-bind (lisp-type lowest highest) (rest range)
          (list lisp-type (or min lowest) (or max highest)))))))

(defun check-property-value (object slot value)
  (let ((check (slot-property-check slot)))
    (when check
      (destructuring-bind (lisp-type min max) check
        (unless (and (typep value lisp-type) (<= min value max))
          (error 'property-value-error
                 :object object :property (slot-property-name slot) :value value
                 :expected (format nil "~:[a number~;an integer~] from ~a to ~a"
                                   (eq lisp-type 'integer) min max)))))))

(defmethod (setf sb-mop:slot-value-using-class) :before
    (value (class gobject-class) object (slot gobject-effective-slot-definition))
  ;; GObject's set_property has already validated what it passes.
  (unless *setting-property*
    (check-property-value object slot value)))

(defmethod (setf sb-mop:slot-value-using-class) :after
    (value (class gobject-class) object (slot gobject-effective-slot-definition))
  (declare (ignore value))
  (when (and (slot-property-name slot)
             (not *setting-property*)
             (not *initializing-slots*)
             (slot-boundp object 'pointer))
    (with-gtk-float-traps
      (%g-object-notify (%object-pointer object) (slot-property-name slot)))))

(defmethod sb-mop:slot-value-using-class :around
    ((class gobject-class) object (slot gobject-effective-slot-definition))
  (if (and (slot-template-child slot)
           (not (sb-mop:slot-boundp-using-class class object slot)))
      (setf (sb-mop:slot-value-using-class class object slot)
            (template-child object (slot-defining-class slot) (slot-template-child slot)))
      (call-next-method)))

(defun template-child (widget class id)
  "The object with ID in CLASS's composite template, as instantiated for WIDGET."
  (let ((p (cffi:foreign-funcall "gtk_widget_get_template_child"
                                 :pointer (object-pointer widget) gtype (class-gtype class)
                                 :string id :pointer)))
    (when (cffi:null-pointer-p p)
      (error "gtk4: ~s's template has no object with id ~s" (class-name class) id))
    (wrap-object p)))

;;; Type designators: the Lisp names for GTypes used in property and signal specs

(defun designator-gtype (designator)
  "The GType for DESIGNATOR: :boolean, :int, :uint, :long, :ulong, :int64,
:uint64, :float, :double, :string, :pointer, :gtype, :variant, :void,
(:enum NAME), (:flags NAME), (:boxed \"GTypeName\"), (:object CLASS), a
GObject class name, or an integer GType."
  (etypecase designator
    (integer designator)
    (keyword
     (ecase designator
       (:void +g-type-none+) (:boolean +g-type-boolean+) (:char +g-type-char+)
       (:uchar +g-type-uchar+) (:int +g-type-int+) (:uint +g-type-uint+)
       (:long +g-type-long+) (:ulong +g-type-ulong+) (:int64 +g-type-int64+)
       (:uint64 +g-type-uint64+) (:float +g-type-float+) (:double +g-type-double+)
       (:string +g-type-string+) (:pointer +g-type-pointer+) (:variant +g-type-variant+)
       (:object +g-type-object+) (:gtype (g-type-gtype))))
    (symbol (class-gtype designator))
    (cons
     (ecase (first designator)
       ((:enum :flags)
        (or (enum-info-gtype (enum-info (second designator)))
            (error "gtk4: ~s has no GType" (second designator))))
       (:object (class-gtype (second designator)))
       (:boxed (or (gtype-from-name (second designator))
                   (error "gtk4: unknown boxed type ~s" (second designator))))))))

;;; Property specs

(defparameter *param-flags*
  '((:readable . 1) (:writable . 2) (:construct . 4) (:construct-only . 8)
    (:explicit-notify . #.(ash 1 30)) (:deprecated . #.(ash 1 31))))

(defun param-flags (flags)
  (reduce #'logior (mapcar (lambda (f) (or (cdr (assoc f *param-flags*))
                                           (error "gtk4: unknown property flag ~s" f)))
                           flags)
          :initial-value 0))

(defun normalize-property-spec (spec)
  "SPEC as (TYPE &key ...): the :property option may be just a type."
  (cond ((keywordp spec) (list spec))
        ((and (consp spec) (member (first spec) '(:enum :flags :object :boxed)))
         (list* (list (first spec) (second spec)) (cddr spec)))
        ((consp spec) spec)
        (t (list spec))))

(defun slot-default (slot)
  "The slot's initform when it is a constant, else NIL."
  (let ((form (sb-mop:slot-definition-initform slot)))
    (and (sb-mop:slot-definition-initfunction slot) (constantp form) (eval form))))

(defun make-pspec (slot)
  "A new GParamSpec for property SLOT."
  (destructuring-bind (type &key nick blurb min max (default nil default-p)
                                 (flags '(:readable :writable)))
      (normalize-property-spec (slot-property slot))
    (let* ((name (slot-property-name slot))
           (nick (or nick name))
           (blurb (or blurb (documentation slot t) ""))
           (default (if default-p default (slot-default slot)))
           (flags (param-flags flags)))
      (flet ((num (v fallback) (or v fallback)))
        (macrolet ((spec (c-name &rest args)
                     `(cffi:foreign-funcall ,c-name :string name :string nick :string blurb
                                            ,@args :int flags :pointer)))
          (if (consp type)
              (ecase (first type)
                (:enum (spec "g_param_spec_enum" gtype (designator-gtype type)
                             :int (if default (enum-value (second type) default)
                                      (cdr (first (enum-info-members (enum-info (second type))))))))
                (:flags (spec "g_param_spec_flags" gtype (designator-gtype type)
                              :uint (if default (enum-value (second type) default) 0)))
                (:object (spec "g_param_spec_object" gtype (designator-gtype type)))
                (:boxed (spec "g_param_spec_boxed" gtype (designator-gtype type))))
              (ecase type
                (:boolean (spec "g_param_spec_boolean" :boolean (and default t)))
                (:int (spec "g_param_spec_int" :int (num min (- (expt 2 31))) :int (num max (1- (expt 2 31)))
                            :int (num default 0)))
                (:uint (spec "g_param_spec_uint" :uint (num min 0) :uint (num max (1- (expt 2 32)))
                             :uint (num default 0)))
                (:long (spec "g_param_spec_long" :long (num min (- (expt 2 63)))
                             :long (num max (1- (expt 2 63))) :long (num default 0)))
                (:ulong (spec "g_param_spec_ulong" :ulong (num min 0) :ulong (num max (1- (expt 2 64)))
                              :ulong (num default 0)))
                (:int64 (spec "g_param_spec_int64" :int64 (num min (- (expt 2 63)))
                              :int64 (num max (1- (expt 2 63))) :int64 (num default 0)))
                (:uint64 (spec "g_param_spec_uint64" :uint64 (num min 0) :uint64 (num max (1- (expt 2 64)))
                               :uint64 (num default 0)))
                (:float (spec "g_param_spec_float" :float (float (num min most-negative-single-float) 1f0)
                              :float (float (num max most-positive-single-float) 1f0)
                              :float (float (num default 0) 1f0)))
                (:double (spec "g_param_spec_double" :double (float (num min most-negative-double-float) 1d0)
                               :double (float (num max most-positive-double-float) 1d0)
                               :double (float (num default 0) 1d0)))
                (:string (spec "g_param_spec_string" :string default))
                (:pointer (spec "g_param_spec_pointer"))
                (:gtype (spec "g_param_spec_gtype" gtype +g-type-none+)))))))))

;;; set_property and get_property

(cffi:defcstruct gobject-class-head
  (g-type gtype)
  (construct-properties :pointer)
  (constructor :pointer)
  (set-property :pointer)
  (get-property :pointer))

(defun pspec-owner-class (pspec)
  (gethash (cffi:foreign-slot-value pspec '(:struct gparam-spec) 'owner-type) *lisp-gtypes*))

(cffi:defcallback lisp-set-property :void
    ((object :pointer) (id :uint) (value :pointer) (pspec :pointer))
  (with-callback-protection ("set_property")
    (let ((slot (aref (class-property-slots (pspec-owner-class pspec)) (1- id)))
          (proxy (wrap-object object)))
      (let ((*setting-property* t))
        (setf (slot-value proxy slot) (gvalue-get value))))))

(cffi:defcallback lisp-get-property :void
    ((object :pointer) (id :uint) (value :pointer) (pspec :pointer))
  (with-callback-protection ("get_property")
    (let ((slot (aref (class-property-slots (pspec-owner-class pspec)) (1- id)))
          (proxy (wrap-object object)))
      (when (slot-boundp proxy slot)
        (set-gvalue value (slot-value proxy slot))))))

(defun class-property-slot-definitions (class)
  "CLASS's own property slots (those it adds, not inherited ones)."
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (remove-if-not (lambda (s)
                   (and (typep s 'gobject-effective-slot-definition)
                        (slot-property-name s)
                        (eq (slot-defining-class s) class)))
                 (sb-mop:class-slots class)))

(defun class-property-specs (class)
  "(NAME SPEC) for each property CLASS defines, for redefinition checks."
  (mapcar (lambda (s) (list (slot-property-name s) (slot-property s)))
          (class-property-slot-definitions class)))

;;; Signals

(defparameter *signal-flags*
  '((:run-first . 1) (:run-last . 2) (:run-cleanup . 4) (:no-recurse . 8)
    (:detailed . 16) (:action . 32) (:no-hooks . 64) (:must-collect . 128)
    (:deprecated . 256)))

(defun install-signal (gtype spec)
  (destructuring-bind (name (&rest arg-types) &key (return :void) (flags '(:run-last))) spec
    (let ((n (length arg-types)))
      (cffi:with-foreign-object (types 'gtype (max n 1))
        (loop for type in arg-types
              for i from 0
              do (setf (cffi:mem-aref types 'gtype i) (designator-gtype type)))
        (cffi:foreign-funcall "g_signal_newv"
                              :string (string-downcase (string name))
                              gtype gtype
                              :int (reduce #'logior
                                           (mapcar (lambda (f) (or (cdr (assoc f *signal-flags*))
                                                                   (error "gtk4: unknown signal flag ~s" f)))
                                                   flags)
                                           :initial-value 0)
                              :pointer (cffi:null-pointer) ; class closure
                              :pointer (cffi:null-pointer) ; accumulator
                              :pointer (cffi:null-pointer)
                              :pointer (cffi:null-pointer) ; marshaller: the generic one
                              gtype (designator-gtype return)
                              :uint n
                              :pointer types
                              :uint)))))

;;; class_init for Lisp-defined classes

(defun initialize-lisp-class (class class-struct)
  "Install CLASS's properties and signals; called from class_init."
  (let ((slots (class-property-slot-definitions class))
        (gtype (class-struct-gtype class-struct)))
    (setf (class-property-slots class)
          (map 'vector #'sb-mop:slot-definition-name slots))
    (when slots
      (setf (cffi:foreign-slot-value class-struct '(:struct gobject-class-head) 'set-property)
            (cffi:callback lisp-set-property)
            (cffi:foreign-slot-value class-struct '(:struct gobject-class-head) 'get-property)
            (cffi:callback lisp-get-property))
      (loop for slot in slots
            for id from 1
            do (cffi:foreign-funcall "g_object_class_install_property"
                                     :pointer class-struct :uint id :pointer (make-pspec slot) :void)))
    (dolist (spec (class-signal-specs class))
      (install-signal gtype spec))))
