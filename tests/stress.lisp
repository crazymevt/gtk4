;;;; stress.lisp — the M1 gate: no leaks under load
;;;;
;;;; Run with `make stress`. *STRESS-SCALE* multiplies every count.

(in-package #:gtk4-tests)

(defvar *stress-scale* 1)

(defun scaled (n) (* n *stress-scale*))

(define-test gtk4-stress)

(defun lisp-side-counts ()
  "Counts that must return to their starting values once garbage is collected."
  (list :handles (rt:handle-count)
        :toggled (hash-table-count rt::*toggled*)
        :strong (hash-table-count rt::*strong-proxies*)))

(defun settled-p (baseline)
  (equal baseline (lisp-side-counts)))

(define-test churn-objects :parent gtk4-stress
  (let ((n (scaled 5000))
        (baseline (lisp-side-counts)))
    (setf *finalized* 0)
    (in-fresh-thread
     (lambda ()
       (dotimes (i n)
         (watch-finalization (make-instance 'gio:simple-action :name "churn")))))
    (true (collect-until (lambda () (= *finalized* n)) :timeout 30)
          "~d of ~d objects finalized" *finalized* n)
    (true (collect-until (lambda () (settled-p baseline)) :timeout 10)
          "counts ~s, expected ~s" (lisp-side-counts) baseline)))

(define-test self-referencing-handlers :parent gtk4-stress
  ;; Each handler closes over its own object: proxy -> C object -> closure ->
  ;; handler -> proxy. Without proxy-owned handlers this cycle never dies.
  (let ((n (scaled 2000))
        (baseline (lisp-side-counts)))
    (setf *finalized* 0)
    (in-fresh-thread
     (lambda ()
       (dotimes (i n)
         (let ((action (make-instance 'gio:simple-action :name "self")))
           (watch-finalization action)
           (rt:connect action "notify::enabled"
                       (lambda (object pspec)
                         (declare (ignore object pspec))
                         (gio:action-get-name action)))))))
    (true (collect-until (lambda () (= *finalized* n)) :timeout 30)
          "~d of ~d objects finalized" *finalized* n)
    (true (collect-until (lambda () (settled-p baseline)) :timeout 10)
          "counts ~s, expected ~s" (lisp-side-counts) baseline)))

(define-test objects-kept-alive-by-c :parent gtk4-stress
  ;; Objects referenced from C must keep their proxies and handlers even when
  ;; Lisp drops every reference, and be freed once C lets go.
  (let* ((n (scaled 1000))
         (baseline (lisp-side-counts))
         (store (gio:list-store-new (rt:class-gtype 'gio:simple-action)))
         (fired 0))
    (setf *finalized* 0)
    (in-fresh-thread
     (lambda ()
       (dotimes (i n)
         (let ((action (make-instance 'gio:simple-action :name "kept" :enabled t)))
           (watch-finalization action)
           (rt:connect action "notify::enabled"
                       (lambda (object pspec)
                         (declare (ignore object pspec))
                         (when (gio:action-get-name action) (incf fired))))
           (gio:list-store-append store action)))))
    (collect-until (lambda () nil) :timeout 1)
    (is = 0 *finalized* "nothing freed while the store holds the objects")
    (dotimes (i n)
      (gio:simple-action-set-enabled (gio:list-model-get-item store i) nil))
    (is = n fired "every handler still runs")
    (gio:list-store-remove-all store)
    (true (collect-until (lambda () (= *finalized* n)) :timeout 30)
          "~d of ~d objects finalized after removal" *finalized* n)
    (setf store nil)
    (true (collect-until (lambda () (settled-p baseline)) :timeout 10)
          "counts ~s, expected ~s" (lisp-side-counts) baseline)))

(define-test callback-churn :parent gtk4-stress
  (let ((n (scaled 3000))
        (baseline (lisp-side-counts))
        (ran 0))
    (dotimes (i n)
      (glib:idle-add glib:+priority-default+ (lambda () (incf ran) nil)))
    (true (iterate-until (lambda () (= ran n)) :timeout 30) "~d of ~d callbacks ran" ran n)
    (true (collect-until (lambda () (settled-p baseline)) :timeout 10)
          "counts ~s, expected ~s" (lisp-side-counts) baseline)))

(define-test signal-connect-disconnect-churn :parent gtk4-stress
  (let ((baseline (lisp-side-counts))
        (action (make-instance 'gio:simple-action :name "churn")))
    (dotimes (i (scaled 5000))
      (rt:disconnect action (rt:connect action "notify" (lambda (o p) (list o p)))))
    (is = 0 (hash-table-count (or (rt::object-handlers action) (make-hash-table))))
    (setf action nil)
    (true (collect-until (lambda () (settled-p baseline)) :timeout 10)
          "counts ~s, expected ~s" (lisp-side-counts) baseline)))

(define-test cross-thread-calls :parent gtk4-stress
  (let* ((per-thread (scaled 200))
         (results (make-array 4 :initial-element 0))
         (workers (loop for w below 4
                        collect (let ((w w))
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (dotimes (i per-thread)
                                       (when (rt:in-main-thread (:wait t)
                                               (gio:action-get-name
                                                (make-instance 'gio:simple-action :name "x")))
                                         (incf (aref results w))))))))))
    (loop while (some #'sb-thread:thread-alive-p workers)
          do (rt:iterate-main-context) (sleep 0.001))
    (mapc #'sb-thread:join-thread workers)
    (is = (* 4 per-thread) (reduce #'+ results))))

(define-test conversion-churn :parent gtk4-stress
  (let ((ok t))
    (dotimes (i (scaled 20000))
      (let* ((data (map '(vector (unsigned-byte 8)) (lambda (c) (logand (+ c i) 255)) #(1 2 3 4 5)))
             (back (glib:base64-decode (glib:base64-encode data))))
        (unless (equalp data back) (setf ok nil)))
      (unless (equal '("a" "b") (%list-copy-strings '("a" "b"))) (setf ok nil)))
    (true ok)))
