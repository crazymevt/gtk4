;;;; objects.lisp — GObject proxies, ownership, properties, signals, errors
;;;;
;;;; Uses GIO types (GSimpleAction, GListStore) because they need no display.

(in-package #:gtk4-tests)

(define-test objects :parent gtk4-tests)

(defclass simple-action (rt:object) ()
  (:metaclass rt:gobject-class)
  (:gtype-name "GSimpleAction")
  (:get-type "g_simple_action_get_type"))

(defclass list-store (rt:object) ()
  (:metaclass rt:gobject-class)
  (:gtype-name "GListStore")
  (:get-type "g_list_store_get_type"))

(defclass tagged-action (simple-action)
  ((tag :initarg :tag :reader tag))
  (:metaclass rt:gobject-class))

;;; Counting finalized C objects with weak references

(defvar *finalized* 0)

(cffi:defcallback count-finalized :void ((data :pointer) (object :pointer))
  (declare (ignore data object))
  (incf *finalized*))

(defun watch-finalization (object)
  (cffi:foreign-funcall "g_object_weak_ref" :pointer (rt:object-pointer object)
                        :pointer (cffi:callback count-finalized)
                        :pointer (cffi:null-pointer) :void))

(defun collect-until (predicate &key (timeout 5))
  "Run full GCs, pending finalizers and the main context until PREDICATE holds."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        do (sb-ext:gc :full t)
           (when (fboundp 'sb-impl::run-pending-finalizers)
             (funcall 'sb-impl::run-pending-finalizers))
           (rt:iterate-main-context)
        until (or (funcall predicate) (> (get-internal-real-time) deadline))
        do (sleep 0.01)
        finally (return (funcall predicate))))

(defun in-fresh-thread (thunk)
  "Run THUNK in a thread and join it, so nothing it allocated stays on our stack."
  (sb-thread:join-thread (sb-thread:make-thread thunk)))

;;; Tests

(define-test make-instance-and-properties :parent objects
  (let ((a (make-instance 'simple-action :name "save" :enabled nil)))
    (is string= "save" (rt:property a :name))
    (false (rt:property a :enabled))
    (setf (rt:property a :enabled) t)
    (true (rt:property a "enabled"))
    (fail (rt:property a :no-such-property))))

(define-test gtype-properties :parent objects
  (let* ((gtype (rt:class-gtype 'simple-action))
         (store (make-instance 'list-store :item-type gtype)))
    (is = gtype (rt:property store :item-type))
    (is string= "GSimpleAction" (rt:gtype-name gtype))))

(define-test lisp-slots-and-properties :parent objects
  (let ((a (make-instance 'tagged-action :tag :primary :name "open")))
    (is eq :primary (tag a))
    (is string= "open" (rt:property a :name))))

(define-test proxy-identity :parent objects
  (let* ((a (make-instance 'simple-action :name "copy"))
         (again (rt:wrap-object (rt:object-pointer a))))
    (is eq a again)))

(define-test unregistered-gtype-uses-ancestor-class :parent objects
  ;; GLocalFile is private to GIO, so no Lisp class exists for it.
  (let ((file (rt:wrap-object (cffi:foreign-funcall "g_file_new_for_path" :string "/tmp" :pointer)
                              :transfer :full)))
    (is eq (find-class 'rt:object) (class-of file))
    (is string= "GLocalFile" (rt:gtype-name (rt:instance-gtype (rt:object-pointer file))))))

(define-test notify-signal :parent objects
  (let* ((a (make-instance 'simple-action :name "paste" :enabled t))
         (seen '())
         (id (rt:connect a "notify::enabled"
                         (lambda (object pspec)
                           (declare (ignore pspec))
                           (push object seen)))))
    (setf (rt:property a :enabled) nil)
    (is = 1 (length seen))
    (is eq a (first seen))
    (true (rt:handler-connected-p a id))
    (rt:disconnect a id)
    (false (rt:handler-connected-p a id))
    (setf (rt:property a :enabled) t)
    (is = 1 (length seen))))

(defvar *symbol-handler-log* '())
(defun symbol-handler (object pspec)
  (declare (ignore object pspec))
  (push :first *symbol-handler-log*))

(define-test symbol-handlers-follow-redefinition :parent objects
  (let ((a (make-instance 'simple-action :name "cut" :enabled t))
        (*symbol-handler-log* '()))
    (rt:connect a "notify::enabled" 'symbol-handler)
    (setf (rt:property a :enabled) nil)
    (let ((old (fdefinition 'symbol-handler)))
      (unwind-protect
           (progn
             (setf (fdefinition 'symbol-handler)
                   (lambda (object pspec)
                     (declare (ignore object pspec))
                     (push :second *symbol-handler-log*)))
             (setf (rt:property a :enabled) t))
        (setf (fdefinition 'symbol-handler) old)))
    (is equal '(:second :first) *symbol-handler-log*)))

(define-test unknown-signal :parent objects
  (fail (rt:connect (make-instance 'simple-action :name "x") :no-such-signal #'identity)))

(define-test handler-errors-do-not-unwind-into-c :parent objects
  (let* ((a (make-instance 'simple-action :name "undo" :enabled t))
         (caught '())
         (rt:*callback-error-handler* (lambda (c where)
                                        (declare (ignore where))
                                        (push c caught))))
    (rt:connect a "notify::enabled" (lambda (o p) (declare (ignore o p)) (error "boom")))
    (setf (rt:property a :enabled) nil)
    (is = 1 (length caught))
    (true (typep (first caught) 'simple-error))))

(define-test gerror-becomes-condition :parent objects
  (let ((condition
          (handler-case
              (cffi:with-foreign-objects ((contents :pointer) (length :size))
                (rt:with-gerror (err)
                  (cffi:foreign-funcall "g_file_get_contents"
                                        :string "/nonexistent/gtk4-test"
                                        :pointer contents :pointer length
                                        :pointer err :boolean))
                nil)
            (rt:glib-error (e) e))))
    (true condition)
    (is string= "g-file-error-quark" (rt:glib-error-domain condition))
    (true (search "nonexistent" (rt:glib-error-message condition)))))

(define-test proxies-release-their-objects :parent objects
  (let ((n 200))
    (setf *finalized* 0)
    (in-fresh-thread
     (lambda ()
       (dotimes (i n)
         (watch-finalization (make-instance 'simple-action :name (format nil "a~d" i))))))
    (true (collect-until (lambda () (= *finalized* n)))
          "all ~d objects finalized (~d were)" n *finalized*)))

(define-test signal-handles-released :parent objects
  (let ((before (rt:handle-count)))
    (in-fresh-thread
     (lambda ()
       (dotimes (i 50)
         (rt:connect (make-instance 'simple-action :name "h") "notify" (lambda (o p) (list o p))))))
    (true (collect-until (lambda () (= (rt:handle-count) before)))
          "handles back to ~d (now ~d)" before (rt:handle-count))))

(define-test in-main-thread-from-worker :parent objects
  (let* ((result nil)
         (worker (sb-thread:make-thread
                  (lambda ()
                    (setf result (rt:in-main-thread (:wait t)
                                   (list (rt:gui-thread-p) (+ 40 2))))))))
    (loop repeat 500
          while (sb-thread:thread-alive-p worker)
          do (rt:iterate-main-context) (sleep 0.01))
    (sb-thread:join-thread worker)
    (is equal '(t 42) result)))

(define-test in-main-thread-propagates-errors :parent objects
  (let* ((failure nil)
         (worker (sb-thread:make-thread
                  (lambda ()
                    (handler-case (rt:in-main-thread (:wait t) (error "from main"))
                      (error (e) (setf failure e)))))))
    (loop repeat 500
          while (sb-thread:thread-alive-p worker)
          do (rt:iterate-main-context) (sleep 0.01))
    (sb-thread:join-thread worker)
    (true (typep failure 'simple-error))))

(define-test self-referencing-handler-is-collected :parent objects
  ;; A handler closing over its own object must not keep it alive forever.
  (setf *finalized* 0)
  (in-fresh-thread
   (lambda ()
     (dotimes (i 20)
       (let ((action (make-instance 'simple-action :name "cycle")))
         (watch-finalization action)
         (rt:connect action "notify" (lambda (o p) (declare (ignore o p)) action))))))
  (true (collect-until (lambda () (= *finalized* 20)))
        "~d of 20 finalized" *finalized*))
