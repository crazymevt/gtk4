;;;; vfuncs.lisp — planning virtual functions
;;;;
;;;; A virtual function is a function pointer in a class (or interface)
;;;; struct. Lisp subclasses override one by storing a trampoline in that
;;;; slot; the trampoline converts the C arguments and calls the Lisp
;;;; implementation. Chaining to the parent calls the parent class's pointer
;;;; with the arguments converted back. Both directions reuse the function
;;;; planner, with a narrower set of argument kinds.

(in-package #:gtk4.generator)

(defparameter *vfunc-in-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype
    :string :strv :object :boxed :record :pointer :enum :flags :glist :gslist)
  "Argument kinds a virtual function may receive.")

(defparameter *vfunc-out-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype
    :enum :flags :object :string)
  "Out-argument kinds a Lisp implementation can fill in.")

(defparameter *vfunc-return-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :float :double :gtype
    :enum :flags :object :boxed :string :strv :record :pointer)
  "Return kinds a Lisp implementation can produce.")

(defvar *vfunc-plans* (make-hash-table :test 'eq)
  "Virtual function gir-callable -> its VFUNC-PLAN, or the reason it has none.")

(defstruct vfunc-plan
  plan                                  ; the function PLAN (args, return, docs)
  owner                                 ; type symbol of the class or interface
  name                                  ; keyword naming the vfunc
  struct                                ; class-struct symbol
  slot)                                 ; the struct slot holding the pointer

(defun class-struct-layout (ctx owner nsname)
  "OWNER's class or interface struct layout, or a reason string."
  (let ((ts (gir-class-type-struct owner)))
    (if ts
        (layout-of ctx (qualify ts nsname))
        "no class struct")))

(defun vfunc-slot (layout vfunc)
  "The slot in LAYOUT holding VFUNC's pointer, or NIL."
  (let ((slot (intern (string-upcase (snake-to-kebab (gir-item-name vfunc))) :keyword)))
    (and (assoc slot (layout-fields layout)) slot)))

(defun vfunc-unsupported (plan)
  "Why the virtual function PLAN cannot be overridden from Lisp, or NIL."
  (loop for (nil spec . options) in (plan-args plan)
        for kind = (spec-kind* spec)
        for direction = (getf options :direction :in)
        do (cond ((or (getf options :user-data-of) (getf options :destroy-of)
                      (getf options :length-of))
                  (return-from vfunc-unsupported "hidden argument (callback data or array length)"))
                 ((getf options :caller-allocates)
                  (return-from vfunc-unsupported "caller-allocated out argument"))
                 ((and (eq direction :out) (not (member kind *vfunc-out-kinds*)))
                  (return-from vfunc-unsupported (format nil "~(~a~) out argument" kind)))
                 ((and (eq direction :out) (eq kind :string)
                       (not (eq (getf options :transfer) :full)))
                  (return-from vfunc-unsupported "borrowed string out argument"))
                 ((and (eq direction :in) (not (member kind *vfunc-in-kinds*)))
                  (return-from vfunc-unsupported (format nil "~(~a~) argument" kind)))))
  (let ((kind (spec-kind* (plan-return plan))))
    (cond ((eq (plan-return plan) :void) nil)
          ((not (member kind *vfunc-return-kinds*))
           (format nil "returns ~(~a~)" kind))
          ((and (member kind '(:string :strv :boxed))
                (not (eq (plan-return-transfer plan) :full)))
           ;; C would keep a pointer into memory nobody owns.
           (format nil "returns a borrowed ~(~a~)" kind)))))

(defun plan-vfunc (ctx ns vfunc owner)
  "A VFUNC-PLAN for VFUNC of OWNER (a class or interface), or (VALUES NIL reason)."
  (let* ((nsname (gir-namespace-name ns))
         (owner-symbol (type-symbol (qualify (gir-item-name owner) nsname)))
         (layout (class-struct-layout ctx owner nsname)))
    (cond
      ((stringp layout) (values nil (format nil "class struct: ~a" layout)))
      ((null (vfunc-slot layout vfunc)) (values nil "no slot in the class struct"))
      (t
       ;; The planner names argument variables in the package of the item's
       ;; symbol; a vfunc has none of its own, so lend it the owner's.
       (setf (gethash vfunc *item-symbols*) owner-symbol)
       (multiple-value-bind (plan reason) (plan-callable ctx ns vfunc owner)
         (remhash vfunc *item-symbols*)
         (cond ((null plan) (values nil reason))
               ((vfunc-unsupported plan) (values nil (vfunc-unsupported plan)))
               (t (make-vfunc-plan
                   :plan plan :owner owner-symbol
                   :name (intern (string-upcase (snake-to-kebab (gir-item-name vfunc))) :keyword)
                   :struct (layout-symbol layout)
                   :slot (vfunc-slot layout vfunc)))))))))

(defun namespace-vfuncs (ctx ns)
  "(VFUNC . OWNER) for every virtual function of NS's GObject classes and interfaces."
  (let ((nsname (gir-namespace-name ns)))
    (loop for c in (gir-namespace-classes ns)
          when (and (member (gir-class-kind c) '(:class :interface))
                    (gobject-type-p ctx (qualify (gir-item-name c) nsname)))
            append (mapcar (lambda (v) (cons v c)) (gir-class-virtual-methods c)))))

(defun vfunc-form (vp nsname)
  (let ((plan (vfunc-plan-plan vp))
        (url (doc-url nsname (format nil "vfunc.~a.~a.html"
                                     (gir-item-name (plan-owner (vfunc-plan-plan vp)))
                                     (gir-item-name (plan-source (vfunc-plan-plan vp)))))))
    `(gtk4.runtime:define-gvfunc (,(vfunc-plan-owner vp) ,(vfunc-plan-name vp))
         (,(vfunc-plan-struct vp) ,(vfunc-plan-slot vp))
       :args ,(loop for (var spec . options) in (plan-args plan)
                    collect `(,var ,spec
                              ,@(when (eq (getf options :direction) :out) '(:direction :out))
                              ,@(unless (member (getf options :transfer) '(nil :none))
                                  `(:transfer ,(getf options :transfer)))))
       ,@(unless (eq (plan-return plan) :void) `(:return ,(plan-return plan)))
       ,@(unless (eq (plan-return-transfer plan) :none)
           `(:return-transfer ,(plan-return-transfer plan)))
       ,@(when (plan-throws plan) '(:throws t))
       ,@(when url `(:url ,url))
       :documentation ,(docstring (plan-doc plan) :url url
                                                  :version (plan-version plan)))))

(defun vfunc-lambda-list (vp)
  "The argument names a DEFINE-VFUNC for VP takes, as text."
  (format nil "~{~(~a~)~^ ~}"
          (loop for (var nil . options) in (plan-args (vfunc-plan-plan vp))
                unless (eq (getf options :direction) :out) collect var)))
