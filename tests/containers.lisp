;;;; containers.lisp — GList, GSList and GHashTable as Lisp values

(in-package #:gtk4-tests)

(define-test containers :parent gtk4-tests)

;;; Test-only bindings to GLib's container functions, which the generated
;;; bindings deliberately leave out, to check values survive a round trip.

(rt:define-gfunction (%list-length "g_list_length")
  :args ((list (:glist :string))) :return :uint)

(rt:define-gfunction (%slist-length "g_slist_length")
  :args ((list (:gslist :int))) :return :uint)

(rt:define-gfunction (%list-copy-strings "g_list_copy")
  :args ((list (:glist :string))) :return (:glist :string) :return-transfer :container)

(rt:define-gfunction (%slist-copy-ints "g_slist_copy")
  :args ((list (:gslist :int))) :return (:gslist :int) :return-transfer :container)

(rt:define-gfunction (%hash-table-size "g_hash_table_size")
  :args ((table (:ghash :string :string))) :return :uint)

(rt:define-gfunction (%hash-table-lookup "g_hash_table_lookup")
  :args ((table (:ghash :string :string)) (key :string)) :return :string)

(define-test list-arguments :parent containers
  (is = 3 (%list-length '("a" "b" "c")))
  (is = 0 (%list-length nil))
  (is = 2 (%slist-length #(10 20))))

(define-test list-round-trips :parent containers
  (is equal '("x" "y" "z") (%list-copy-strings '("x" "y" "z")))
  (is equal '(1 -2 300) (%slist-copy-ints '(1 -2 300))))

(define-test list-returns-from-gio :parent containers
  ;; g_file_enumerator_next_files_finish returns a GList of GFileInfo (transfer full).
  (let* ((dir (gio:file-new-for-path "/"))
         (enumerator (gio:file-enumerate-children dir "standard::name" nil nil))
         (infos :pending))
    (gio:file-enumerator-next-files-async
     enumerator 5 glib:+priority-default+ nil
     (lambda (source result)
       (setf infos (gio:file-enumerator-next-files-finish source result))))
    (true (iterate-until (lambda () (not (eq infos :pending)))))
    (true (consp infos))
    (true (every (lambda (i) (typep i 'gio:file-info)) infos))
    (true (every #'stringp (mapcar #'gio:file-info-get-name infos)))))

(define-test hash-table-arguments :parent containers
  (is = 2 (%hash-table-size '(("a" . "1") ("b" . "2"))))
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "k" table) "v")
    (is string= "v" (%hash-table-lookup table "k"))))

(define-test hash-table-returns :parent containers
  (let ((params (glib:uri-parse-params "a=1&b=two" -1 "&" nil)))
    (true (hash-table-p params))
    (is = 2 (hash-table-count params))
    (is string= "1" (gethash "a" params))
    (is string= "two" (gethash "b" params))))
