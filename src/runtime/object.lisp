;;;; object.lisp — GObject proxies, ownership and properties
;;;;
;;;; Every GObject seen by Lisp has exactly one proxy, found through a
;;;; weak-value table keyed by address.
;;;;
;;;; Lisp holds each wrapped object through one GObject *toggle reference*.
;;;; GObject tells us when that becomes the only reference left:
;;;;   - other references exist (say, a widget inside a window): the proxy is
;;;;     also kept in a strong table, so it and its Lisp state stay alive;
;;;;   - only ours is left: the proxy is held weakly, and once Lisp drops it
;;;;     too it is collected, its finalizer queues the toggle reference to be
;;;;     removed on the GUI thread, and the C object is freed.
;;;; Signal handlers belong to their proxy (the global handle table only
;;;; points at them weakly), so a handler that refers to its own object does
;;;; not keep the pair alive forever.

(in-package #:gtk4.runtime)

;;; Release queue

(defvar *release-queue* nil)
(defvar *release-lock* (sb-thread:make-mutex :name "gtk4 release queue"))
(defvar *release-scheduled* nil)

(cffi:defcallback drain-release-queue :int ((data :pointer))
  (declare (ignore data))
  (let ((items (sb-thread:with-mutex (*release-lock*)
                 (setf *release-scheduled* nil)
                 (shiftf *release-queue* nil))))
    (with-gtk-float-traps
      (loop for (kind address gtype) in items
            for pointer = (cffi:make-pointer address)
            do (ecase kind
                 (:toggle (release-toggle-reference address))
                 (:boxed (%g-boxed-free gtype pointer))
                 (:free (%g-free pointer))))))
  0)

(defun enqueue-release (kind address &optional gtype)
  "Queue the reference at ADDRESS to be released on the main context.
Safe to call from any thread, including SBCL's finalizer thread."
  (sb-thread:with-mutex (*release-lock*)
    (push (list kind address gtype) *release-queue*)
    (unless *release-scheduled*
      (setf *release-scheduled* t)
      (%g-idle-add (cffi:callback drain-release-queue) (cffi:null-pointer)))))

;;; Metaclass

(defvar *gtype-name-classes* (make-hash-table :test 'equal)
  "GType name -> the Lisp class registered for it.")

(defvar *gtype-classes* (make-hash-table)
  "GType -> Lisp class to instantiate for it (nearest registered ancestor), cached.")

(defclass gobject-class (standard-class)
  ((gtype-name :initform nil :reader class-gtype-name)
   (get-type :initform nil :reader class-get-type)
   (gtype :initform nil))
  (:documentation "Metaclass linking a Lisp class to a GType. Class options:
  (:gtype-name \"GtkButton\")         the GType's name
  (:get-type \"gtk_button_get_type\")  its registration function, called on first use"))

(defmethod sb-mop:validate-superclass ((class gobject-class) (super standard-class)) t)

(defmethod shared-initialize :after ((class gobject-class) slot-names
                                     &key gtype-name get-type &allow-other-keys)
  (declare (ignore slot-names))
  (when gtype-name
    (setf (slot-value class 'gtype-name) (first gtype-name)
          (slot-value class 'gtype) nil
          (gethash (first gtype-name) *gtype-name-classes*) class)
    (clrhash *gtype-classes*))
  (when get-type
    (setf (slot-value class 'get-type) (first get-type))))

(defun class-gtype (class)
  "The GType for CLASS (a class or class name), registering it if needed."
  (let ((class (if (symbolp class) (find-class class) class)))
    (or (slot-value class 'gtype)
        (setf (slot-value class 'gtype)
              (let ((named (gtype-defining-class class)))
                (or (gtype-from-name (class-gtype-name named) (class-get-type named))
                    (error "gtk4: GType ~s is not registered" (class-gtype-name named))))))))

(defun gtype-defining-class (class)
  "The nearest class in CLASS's precedence list that names a GType. A Lisp
subclass without its own :gtype-name shares its parent's GType."
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (or (find-if (lambda (c) (and (typep c 'gobject-class) (class-gtype-name c)))
               (sb-mop:class-precedence-list class))
      (error "gtk4: ~s has no GType" (class-name class))))

(defun lisp-class-for-gtype (gtype)
  "The Lisp class for proxies of GTYPE: its own class if one is registered,
otherwise the nearest registered ancestor's."
  (or (gethash gtype *gtype-classes*)
      (setf (gethash gtype *gtype-classes*)
            (loop for g = gtype then (gtype-parent g)
                  while g
                  for class = (gethash (gtype-name g) *gtype-name-classes*)
                  when class return class
                  finally (error "gtk4: no Lisp class for GType ~a" (gtype-name gtype))))))

;;; Base classes

(defclass object ()
  ((pointer :reader %object-pointer)
   (handlers :initform nil :accessor object-handlers
             :documentation "Signal handlers connected through this proxy, by handle
address. Keeping them here (not in the handle table) ties their lifetime to the proxy's."))
  (:metaclass gobject-class)
  (:gtype-name "GObject")
  (:get-type "g_object_get_type")
  (:documentation "Proxy for a GObject. Each C instance has at most one proxy."))

(defclass initially-unowned (object) ()
  (:metaclass gobject-class)
  (:gtype-name "GInitiallyUnowned")
  (:get-type "g_initially_unowned_get_type"))

(defmethod print-object ((o object) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (if (slot-boundp o 'pointer)
        (format stream "~a ~x" (gtype-name (instance-gtype (%object-pointer o)))
                (cffi:pointer-address (%object-pointer o)))
        (format stream "(no instance)"))))

;;; Boxed proxies

(defclass boxed ()
  ((pointer :initarg :pointer :reader %boxed-pointer)
   (gtype :initarg :gtype :reader boxed-gtype))
  (:documentation "Proxy owning a copy of a GBoxed value."))

(defmethod print-object ((b boxed) stream)
  (print-unreadable-object (b stream :type t)
    (format stream "~a ~x" (gtype-name (boxed-gtype b))
            (cffi:pointer-address (%boxed-pointer b)))))

(defvar *boxed-classes* (make-hash-table :test 'equal)
  "GType name -> Lisp class for boxed proxies; default BOXED.")

(defun wrap-boxed (pointer gtype &key (transfer :none))
  "A BOXED proxy for POINTER. With transfer :NONE the value is copied first,
so the proxy always owns what it points to."
  (unless (cffi:null-pointer-p pointer)
    (let* ((owned (if (eq transfer :full) pointer (%g-boxed-copy gtype pointer)))
           (class (gethash (gtype-name gtype) *boxed-classes* 'boxed))
           (proxy (make-instance class :pointer owned :gtype gtype))
           (address (cffi:pointer-address owned)))
      (sb-ext:finalize proxy (lambda () (enqueue-release :boxed address gtype))
                       :dont-save t)
      proxy)))

;;; Plain struct proxies

(defclass record ()
  ((pointer :initarg :pointer :reader %record-pointer))
  (:documentation "Proxy owning a g_malloc'd copy of a plain C struct (one without a GType)."))

(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~x" (cffi:pointer-address (%record-pointer r)))))

(defun copy-record (pointer class size)
  "A new CLASS proxy owning a copy of the SIZE-byte struct at POINTER."
  (let ((copy (cffi:foreign-funcall "g_malloc0" :size (max size 1) :pointer)))
    (cffi:foreign-funcall "memcpy" :pointer copy :pointer pointer :size size :pointer)
    (let ((proxy (make-instance class :pointer copy))
          (address (cffi:pointer-address copy)))
      (sb-ext:finalize proxy (lambda () (enqueue-release :free address)) :dont-save t)
      proxy)))

;;; Pointers

(defun object-pointer (thing)
  "The C pointer behind THING: a proxy, a raw foreign pointer, or NIL (NULL)."
  (cond ((null thing) (cffi:null-pointer))
        ((cffi:pointerp thing) thing)
        ((typep thing 'object) (%object-pointer thing))
        ((typep thing 'boxed) (%boxed-pointer thing))
        ((typep thing 'record) (%record-pointer thing))
        (t (error "gtk4: ~s is not a GObject, boxed value or pointer" thing))))

;;; Wrapping C pointers

(defvar *proxies* (make-hash-table :weakness :value :synchronized t)
  "Instance address -> its proxy.")

(defun proxy-count () (hash-table-count *proxies*))

(cffi:defcfun ("g_object_add_toggle_ref" %g-object-add-toggle-ref) :void
  (object :pointer) (notify :pointer) (data :pointer))
(cffi:defcfun ("g_object_remove_toggle_ref" %g-object-remove-toggle-ref) :void
  (object :pointer) (notify :pointer) (data :pointer))

(defvar *strong-proxies* (make-hash-table :synchronized t)
  "Address -> proxy, for objects that C code also references.")

(defvar *toggled* (make-hash-table :synchronized t)
  "Addresses on which Lisp holds its toggle reference. It outlives any one
proxy: a new proxy for the same object reuses it.")

(defun object-ref-count (pointer)
  ;; GObject's public layout: GTypeInstance, then guint ref_count.
  (cffi:mem-ref pointer :uint (cffi:foreign-type-size :pointer)))

(cffi:defcallback toggle-notify :void ((data :pointer) (object :pointer) (is-last-ref :boolean))
  (declare (ignore data))
  (let ((address (cffi:pointer-address object)))
    (if is-last-ref
        (remhash address *strong-proxies*)
        (let ((proxy (gethash address *proxies*)))
          (when proxy (setf (gethash address *strong-proxies*) proxy))))))

(defun acquire-toggle-reference (pointer transfer)
  "Give Lisp its toggle reference on POINTER, consuming a reference handed
over with TRANSFER :FULL (or a floating one)."
  (let ((address (cffi:pointer-address pointer))
        (owned (cond ((%g-object-is-floating pointer) (%g-object-ref-sink pointer) t)
                     ((eq transfer :full) t))))
    (if (gethash address *toggled*)
        ;; Still held from an earlier proxy whose release is pending.
        (when owned (%g-object-unref pointer))
        (progn
          (setf (gethash address *toggled*) t)
          (%g-object-add-toggle-ref pointer (cffi:callback toggle-notify) (cffi:null-pointer))
          (when owned (%g-object-unref pointer))))))

(defun release-toggle-reference (address)
  "Called on the GUI thread after a proxy was collected. Does nothing if a new
proxy for the object exists by now, or the reference is already gone."
  (unless (or (gethash address *proxies*) (not (gethash address *toggled*)))
    (remhash address *toggled*)
    (remhash address *strong-proxies*)
    (%g-object-remove-toggle-ref (cffi:make-pointer address)
                                 (cffi:callback toggle-notify) (cffi:null-pointer))))

(defun register-proxy (proxy pointer transfer)
  "Record PROXY for POINTER, take Lisp's reference, and hold the proxy
strongly if C code holds the object too."
  (let ((address (cffi:pointer-address pointer)))
    (setf (gethash address *proxies*) proxy)
    (acquire-toggle-reference pointer transfer)
    (when (> (object-ref-count pointer) 1)
      (setf (gethash address *strong-proxies*) proxy))
    (sb-ext:finalize proxy (lambda () (enqueue-release :toggle address))
                     :dont-save t)
    proxy))

(defun wrap-object (pointer &key (transfer :none))
  "The proxy for the GObject at POINTER, creating it if needed. TRANSFER says
whether the caller hands over a reference (:FULL) or not (:NONE), as in the
GIR annotation. NULL gives NIL."
  (unless (cffi:null-pointer-p pointer)
    (let ((existing (gethash (cffi:pointer-address pointer) *proxies*)))
      (cond
        (existing
         ;; The proxy already holds Lisp's reference; drop the one handed to us.
         (when (eq transfer :full) (%g-object-unref pointer))
         existing)
        (t
         (let ((proxy (allocate-instance (lisp-class-for-gtype (instance-gtype pointer)))))
           (setf (slot-value proxy 'pointer) pointer)
           (shared-initialize proxy t)
           (register-proxy proxy pointer transfer)))))))

;;; Properties

(defun property-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun find-pspec (class-struct name gtype)
  (let ((pspec (%g-object-class-find-property class-struct name)))
    (when (cffi:null-pointer-p pspec)
      (error "gtk4: ~a has no property ~s" (gtype-name gtype) name))
    pspec))

(defun pspec-value-type (pspec)
  (cffi:foreign-slot-value pspec '(:struct gparam-spec) 'value-type))

(defun property (object name)
  "The value of OBJECT's GObject property NAME (a keyword or string)."
  (let* ((pointer (object-pointer object))
         (name (property-name name))
         (pspec (find-pspec (instance-class pointer) name (instance-gtype pointer))))
    (with-gvalue (v (pspec-value-type pspec))
      (%g-object-get-property pointer name v)
      (gvalue-get v))))

(defun (setf property) (value object name)
  (let* ((pointer (object-pointer object))
         (name (property-name name))
         (pspec (find-pspec (instance-class pointer) name (instance-gtype pointer))))
    (with-gvalue (v (pspec-value-type pspec))
      (set-gvalue v value)
      (with-gtk-float-traps (%g-object-set-property pointer name v))
      value)))

;;; Creating instances with MAKE-INSTANCE

(defun slot-initargs (class)
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (loop for slot in (sb-mop:class-slots class)
        append (sb-mop:slot-definition-initargs slot)))

(defun split-initargs (class initargs)
  "Split INITARGS into Lisp slot initargs and GObject property initargs."
  (let ((slot-keys (slot-initargs class)) slots props)
    (loop for (key value) on initargs by #'cddr
          do (if (member key slot-keys)
                 (setf slots (list* key value slots))
                 (setf props (list* key value props))))
    (values slots props)))

(defun new-gobject (gtype props)
  "Call g_object_new_with_properties for GTYPE with PROPS, a plist of
property names and Lisp values. Returns the new instance pointer."
  (let* ((class-struct (%g-type-class-ref gtype))
         (pairs (loop for (k v) on props by #'cddr collect (cons (property-name k) v)))
         (n (length pairs)))
    (cffi:with-foreign-objects ((names :pointer (max n 1))
                                (gvalues '(:struct gvalue) (max n 1)))
      (dotimes (i (* 3 (max n 1))) (setf (cffi:mem-aref gvalues :uint64 i) 0))
      (let ((initialized 0) (strings '()))
        (unwind-protect
             (progn
               (loop for (name . value) in pairs
                     for i from 0
                     for gv = (cffi:inc-pointer gvalues (* i +gvalue-size+))
                     for pspec = (find-pspec class-struct name gtype)
                     do (let ((s (cffi:foreign-string-alloc name)))
                          (push s strings)
                          (setf (cffi:mem-aref names :pointer i) s))
                        (%g-value-init gv (pspec-value-type pspec))
                        (incf initialized)
                        (set-gvalue gv value))
               (with-gtk-float-traps
                 (%g-object-new-with-properties gtype n names gvalues)))
          (dotimes (i initialized)
            (%g-value-unset (cffi:inc-pointer gvalues (* i +gvalue-size+))))
          (mapc #'cffi:foreign-string-free strings))))))

(defmethod initialize-instance :around ((object object) &rest initargs
                                        &key &allow-other-keys)
  "MAKE-INSTANCE on a GObject class creates the C instance, passing initargs
that are not Lisp slot initargs as GObject properties."
  (multiple-value-bind (slot-args props) (split-initargs (class-of object) initargs)
    (let ((pointer (new-gobject (class-gtype (class-of object)) props)))
      (setf (slot-value object 'pointer) pointer)
      (register-proxy object pointer :full)
      (apply #'call-next-method object slot-args))))
