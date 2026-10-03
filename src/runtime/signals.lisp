;;;; signals.lisp — connecting Lisp functions to GObject signals

(in-package #:gtk4.runtime)

(cffi:defcfun ("g_signal_parse_name" %g-signal-parse-name) :boolean
  (detailed-signal :string) (itype gtype) (signal-id :pointer) (detail :pointer)
  (force-detail-quark :boolean))

(defun resolve-handler (handler)
  "A symbol is looked up on every call, so redefining it at the REPL takes
effect in a running program."
  (if (symbolp handler) (fdefinition handler) handler))

(cffi:defcallback marshal-lisp-closure :void
    ((closure :pointer) (return-value :pointer) (n-params :uint) (params :pointer)
     (hint :pointer) (marshal-data :pointer))
  (declare (ignore hint marshal-data))
  (with-callback-protection ("signal handler")
    (let* ((handler (handle-value (cffi:foreign-slot-value closure '(:struct gclosure) 'data)))
           (args (loop for i below n-params
                       collect (gvalue-get (cffi:inc-pointer params (* i +gvalue-size+)))))
           (result (apply (resolve-handler handler) args)))
      (when (and (not (cffi:null-pointer-p return-value))
                 (/= 0 (gvalue-type return-value)))
        (set-gvalue return-value result)))))

(defun signal-name (signal)
  (etypecase signal
    (string signal)
    (symbol (string-downcase (symbol-name signal)))))

(defun connect (object signal handler &key after)
  "Call HANDLER (a function, or a symbol naming one) whenever OBJECT emits
SIGNAL (a keyword like :clicked, or a string like \"notify::label\").
HANDLER receives the emitting object followed by the signal's arguments;
its value becomes the signal's return value. Returns the handler id."
  (let* ((pointer (object-pointer object))
         (name (signal-name signal)))
    (cffi:with-foreign-objects ((id :uint) (detail :uint32))
      (unless (%g-signal-parse-name name (instance-gtype pointer) id detail t)
        (error "gtk4: ~a has no signal ~s" (gtype-name (instance-gtype pointer)) name)))
    (let* ((data (make-handle handler))
           (closure (%g-closure-new-simple (cffi:foreign-type-size '(:struct gclosure)) data)))
      (%g-closure-set-marshal closure (cffi:callback marshal-lisp-closure))
      (%g-closure-add-finalize-notifier closure data (cffi:callback free-handle-closure-notify))
      (%g-signal-connect-closure pointer name closure after))))

(defun disconnect (object handler-id)
  (%g-signal-handler-disconnect (object-pointer object) handler-id))

(defun block-handler (object handler-id)
  (%g-signal-handler-block (object-pointer object) handler-id))

(defun unblock-handler (object handler-id)
  (%g-signal-handler-unblock (object-pointer object) handler-id))

(defun handler-connected-p (object handler-id)
  (%g-signal-handler-is-connected (object-pointer object) handler-id))
