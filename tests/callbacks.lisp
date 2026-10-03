;;;; callbacks.lisp — Lisp functions passed to C as callbacks, one test per GIR scope

(in-package #:gtk4-tests)

(define-test callbacks :parent gtk4-tests)

(defun iterate-until (predicate &key (timeout 5))
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        do (rt:iterate-main-context)
        until (or (funcall predicate) (> (get-internal-real-time) deadline))
        do (sleep 0.005)
        finally (return (funcall predicate))))

(define-test notified-scope-idle-handler :parent callbacks
  ;; g_idle_add_full: scope "notified". The source runs until the function
  ;; returns false; GLib then calls the destroy notifier, freeing the handle.
  (let ((before (rt:handle-count))
        (calls 0))
    (glib:idle-add glib:+priority-default+ (lambda () (< (incf calls) 3)))
    (is = (1+ before) (rt:handle-count))
    (true (iterate-until (lambda () (= calls 3))))
    (true (iterate-until (lambda () (= (rt:handle-count) before))))))

(define-test async-scope-file-read :parent callbacks
  ;; g_file_read_async: scope "async". The trampoline frees the handle after
  ;; its one call.
  (let ((before (rt:handle-count))
        (file (gio:file-new-for-path "/etc/hosts"))
        (stream nil))
    (gio:file-read-async file glib:+priority-default+ nil
                         (lambda (source result)
                           (setf stream (gio:file-read-finish source result))))
    (true (iterate-until (lambda () stream)))
    (true (typep stream 'gio:file-input-stream))
    (is = before (rt:handle-count))))

(define-test async-errors-reach-the-callback :parent callbacks
  (let ((file (gio:file-new-for-path "/nonexistent/gtk4/callback-test"))
        (outcome nil))
    (gio:file-read-async file glib:+priority-default+ nil
                         (lambda (source result)
                           (setf outcome (handler-case (gio:file-read-finish source result)
                                           (rt:glib-error (e) e)))))
    (true (iterate-until (lambda () outcome)))
    (true (typep outcome 'rt:glib-error))))

(define-test call-scope-sort :parent callbacks
  ;; g_list_store_sort: scope "call". The handle is freed when the C call returns.
  (let* ((before (rt:handle-count))
         (store (gio:list-store-new (rt:class-gtype 'gio:simple-action))))
    (dolist (name '("delta" "alpha" "charlie" "bravo"))
      (gio:list-store-append store (make-instance 'gio:simple-action :name name)))
    (gio:list-store-sort store
                         (lambda (a b)
                           (let ((x (gio:action-get-name (rt:wrap-object a)))
                                 (y (gio:action-get-name (rt:wrap-object b))))
                             (cond ((string< x y) -1) ((string> x y) 1) (t 0)))))
    (is equal '("alpha" "bravo" "charlie" "delta")
        (loop for i below (gio:list-model-get-n-items store)
              collect (gio:action-get-name (gio:list-model-get-item store i))))
    (is = before (rt:handle-count))))

(define-test callback-errors-are-contained :parent callbacks
  (let* ((caught '())
         (rt:*callback-error-handler* (lambda (c where) (declare (ignore where)) (push c caught)))
         (ran nil))
    (glib:idle-add glib:+priority-default+ (lambda () (setf ran t) (error "inside idle")))
    (true (iterate-until (lambda () ran)))
    (is = 1 (length caught))))

(defvar *redefinable-callback-log* '())
(defun redefinable-idle () (push :original *redefinable-callback-log*) nil)

(define-test symbol-callbacks :parent callbacks
  (let ((*redefinable-callback-log* '()))
    (glib:idle-add glib:+priority-default+ 'redefinable-idle)
    (true (iterate-until (lambda () *redefinable-callback-log*)))
    (is equal '(:original) *redefinable-callback-log*)))
