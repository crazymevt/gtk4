;;;; signals.lisp — connecting Lisp functions to GObject signals

(in-package #:gtk4.runtime)

(cffi:defcfun ("g_signal_parse_name" %g-signal-parse-name) :boolean
  (detailed-signal :string) (itype gtype) (signal-id :pointer) (detail :pointer)
  (force-detail-quark :boolean))

(defun resolve-handler (handler)
  "A symbol is looked up on every call, so redefining it at the REPL takes
effect in a running program."
  (if (symbolp handler) (fdefinition handler) handler))

;;; A signal handler's handle holds a SIGNAL-HANDLER: a weak pointer to the
;;; handler (the proxy's HANDLERS table holds it strongly) and the address of
;;; the object it was connected on.

(defstruct (signal-handler (:constructor make-signal-handler (weak address)))
  weak address)

(defun signal-handler-function (handle)
  (let ((entry (handle-value handle)))
    (and entry (sb-ext:weak-pointer-value (signal-handler-weak entry)))))

(cffi:defcallback free-signal-handler :void ((data :pointer) (closure :pointer))
  (declare (ignore closure))
  (let ((entry (handle-value data)))
    (when entry
      (let ((proxy (gethash (signal-handler-address entry) *proxies*)))
        (when (and proxy (object-handlers proxy))
          (remhash (cffi:pointer-address data) (object-handlers proxy))))))
  (free-handle data))

(cffi:defcallback marshal-lisp-closure :void
    ((closure :pointer) (return-value :pointer) (n-params :uint) (params :pointer)
     (hint :pointer) (marshal-data :pointer))
  (declare (ignore hint marshal-data))
  (with-callback-protection ("signal handler")
    (let ((handler (signal-handler-function
                    (cffi:foreign-slot-value closure '(:struct gclosure) 'data))))
      ;; NIL only while an object is being freed after its proxy was collected.
      (when handler
        (let* ((args (loop for i below n-params
                           collect (gvalue-get (cffi:inc-pointer params (* i +gvalue-size+)))))
               (result (apply (resolve-handler handler) args)))
          (when (and (not (cffi:null-pointer-p return-value))
                     (/= 0 (gvalue-type return-value)))
            (set-gvalue return-value result)))))))

(defun signal-name (signal)
  (etypecase signal
    (string signal)
    (symbol (string-downcase (symbol-name signal)))))

(defun connect (object signal handler &key after)
  "Call HANDLER (a function, or a symbol naming one) whenever OBJECT emits
SIGNAL (a keyword like :clicked, or a string like \"notify::label\").
HANDLER receives the emitting object followed by the signal's arguments;
its value becomes the signal's return value. Returns the handler id."
  (let* ((proxy (if (typep object 'object) object (wrap-object (object-pointer object))))
         (pointer (object-pointer proxy))
         (name (signal-name signal)))
    (cffi:with-foreign-objects ((id :uint) (detail :uint32))
      (unless (%g-signal-parse-name name (instance-gtype pointer) id detail t)
        (error "gtk4: ~a has no signal ~s" (gtype-name (instance-gtype pointer)) name)))
    (let* ((data (make-handle (make-signal-handler (sb-ext:make-weak-pointer handler)
                                                   (cffi:pointer-address pointer))))
           (closure (%g-closure-new-simple (cffi:foreign-type-size '(:struct gclosure)) data)))
      (unless (object-handlers proxy)
        (setf (object-handlers proxy) (make-hash-table :synchronized t)))
      (setf (gethash (cffi:pointer-address data) (object-handlers proxy)) handler)
      (%g-closure-set-marshal closure (cffi:callback marshal-lisp-closure))
      (%g-closure-add-finalize-notifier closure data (cffi:callback free-signal-handler))
      (%g-signal-connect-closure pointer name closure after))))

(defun disconnect (object handler-id)
  (%g-signal-handler-disconnect (object-pointer object) handler-id))

(defun block-handler (object handler-id)
  (%g-signal-handler-block (object-pointer object) handler-id))

(defun unblock-handler (object handler-id)
  (%g-signal-handler-unblock (object-pointer object) handler-id))

(defun handler-connected-p (object handler-id)
  (%g-signal-handler-is-connected (object-pointer object) handler-id))

;;; Emitting

(cffi:defcstruct gsignal-query
  (signal-id :uint)
  (signal-name :pointer)
  (itype gtype)
  (signal-flags :uint)
  (return-type gtype)
  (n-params :uint)
  (param-types :pointer))

(cffi:defcfun ("g_signal_query" %g-signal-query) :void (signal-id :uint) (query :pointer))
(cffi:defcfun ("g_signal_emitv" %g-signal-emitv) :void
  (instance-and-params :pointer) (signal-id :uint) (detail :uint32) (return-value :pointer))

(defun emit (object signal &rest args)
  "Emit SIGNAL (a keyword or \"name::detail\" string) on OBJECT with ARGS,
converted to the signal's parameter types. Returns the signal's return value."
  (let* ((pointer (object-pointer object))
         (gtype (instance-gtype pointer))
         (name (signal-name signal)))
    (cffi:with-foreign-objects ((id :uint) (detail :uint32)
                                (query '(:struct gsignal-query)))
      (unless (%g-signal-parse-name name gtype id detail t)
        (error "gtk4: ~a has no signal ~s" (gtype-name gtype) name))
      (%g-signal-query (cffi:mem-ref id :uint) query)
      (let* ((n (cffi:foreign-slot-value query '(:struct gsignal-query) 'n-params))
             (types (cffi:foreign-slot-value query '(:struct gsignal-query) 'param-types))
             (return-type (cffi:foreign-slot-value query '(:struct gsignal-query) 'return-type)))
        (unless (= n (length args))
          (error "gtk4: signal ~s takes ~d argument~:p, got ~d" name n (length args)))
        (cffi:with-foreign-objects ((values '(:struct gvalue) (1+ n))
                                    (ret '(:struct gvalue)))
          (dotimes (i (* 3 (+ n 2)))
            (when (< i (* 3 (1+ n))) (setf (cffi:mem-aref values :uint64 i) 0)))
          (dotimes (i 3) (setf (cffi:mem-aref ret :uint64 i) 0))
          (flet ((slot (i) (cffi:inc-pointer values (* i +gvalue-size+))))
            (%g-value-init (slot 0) gtype)
            (%g-value-set-object (slot 0) pointer)
            (loop for arg in args
                  for i from 1
                  ;; The low bit marks G_SIGNAL_TYPE_STATIC_SCOPE, not part of the type.
                  for type = (logandc2 (cffi:mem-aref types 'gtype (1- i)) 1)
                  do (%g-value-init (slot i) type)
                     (set-gvalue (slot i) arg))
            (let ((has-return (/= return-type +g-type-none+)))
              (when has-return (%g-value-init ret (logandc2 return-type 1)))
              (unwind-protect
                   (progn
                     (with-gtk-float-traps
                       (%g-signal-emitv values (cffi:mem-ref id :uint) (cffi:mem-ref detail :uint32)
                                        (if has-return ret (cffi:null-pointer))))
                     (and has-return (gvalue-get ret)))
                (dotimes (i (1+ n)) (%g-value-unset (slot i)))
                (when has-return (%g-value-unset ret))))))))))
