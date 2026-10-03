;;;; arrays.lisp — C arrays in every direction

(in-package #:gtk4-tests)

(define-test arrays :parent gtk4-tests)

(defun octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(define-test in-array-with-length :parent arrays
  ;; (data, length): the length parameter is hidden and filled in.
  (is string= "900150983cd24fb0d6963f7d28e17f72"
      (glib:compute-checksum-for-data :md5 (octets "abc")))
  (is string= "YWJj" (glib:base64-encode (octets "abc")))
  ;; Any sequence works as input.
  (is string= "YWJj" (glib:base64-encode '(97 98 99))))

(define-test return-array-with-out-length :parent arrays
  (let ((decoded (glib:base64-decode "YWJj")))
    (true (typep decoded '(vector (unsigned-byte 8))))
    (is equalp (octets "abc") decoded)))

(define-test zero-terminated-return :parent arrays
  (let ((dirs (glib:get-system-data-dirs)))
    (true (consp dirs))
    (true (every #'stringp dirs))))

(define-test gtype-array-return :parent arrays
  (let ((children (gobject:type-children (rt:class-gtype 'rt:initially-unowned))))
    (true (listp children))
    (true (every #'integerp children))))

(define-test object-array-argument :parent arrays
  (let ((store (gio:list-store-new (rt:class-gtype 'gio:simple-action))))
    (gio:list-store-splice store 0 0
                           (list (make-instance 'gio:simple-action :name "one")
                                 (make-instance 'gio:simple-action :name "two")))
    (is = 2 (gio:list-model-get-n-items store))
    (is string= "two" (gio:action-get-name (gio:list-model-get-item store 1)))))

(define-test caller-allocated-buffer :parent arrays
  ;; g_input_stream_read: the caller passes COUNT and gets the filled buffer back.
  (let* ((bytes (glib:bytes-new (octets "hello")))
         (stream (gio:memory-input-stream-new-from-bytes bytes)))
    (multiple-value-bind (n buffer) (gio:input-stream-read stream 3 nil)
      (is = 3 n)
      (is equalp (octets "hel") buffer))))

(define-test string-array-out-parameter :parent arrays
  (let ((kf (glib:key-file-new))
        (data (format nil "[g]~%a=1~%b=2~%")))
    (glib:key-file-load-from-data kf data (length data) nil)
    (is equal '("a" "b") (glib:key-file-get-keys kf "g"))))
