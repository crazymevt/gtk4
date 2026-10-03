;;;; define.lisp — the macros generated code is written in
;;;;
;;;; The generator emits calls to these macros, never their expansions, so
;;;; how a binding is compiled can change without regenerating anything.
;;;;
;;;; Marshalling specs used in DEFINE-GFUNCTION argument and return lists:
;;;;   :boolean :int8 … :uint64 :int :uint :long :ulong :size :ssize
;;;;   :float :double :pointer :void   passed as the CFFI type of that name
;;;;   :gtype                          a GType integer
;;;;   :string                         Lisp string <-> UTF-8 char*
;;;;   :strv                           list of strings <-> NULL-terminated char**
;;;;   (:object class)                 GObject proxy
;;;;   (:boxed gtype-name get-type)    GBoxed proxy
;;;;   (:record name)                  plain C struct, passed as a raw pointer
;;;;   (:enum name) (:flags name)      keywords (lists of keywords for flags)

(in-package #:gtk4.runtime)

;;; Lazily resolved C functions

(define-condition unavailable-function (error)
  ((name :initarg :name :reader unavailable-function-name)
   (version :initarg :version :initform nil :reader unavailable-function-version))
  (:report (lambda (c s)
             (format s "~a is not in the loaded libraries~@[; it needs version ~a~]."
                     (unavailable-function-name c) (unavailable-function-version c)))))

(defstruct (fcell (:constructor make-fcell (name &optional version)))
  name version (pointer nil))

(declaim (inline fcell-address))
(defun fcell-address (cell)
  (or (fcell-pointer cell)
      (setf (fcell-pointer cell)
            (or (cffi:foreign-symbol-pointer (fcell-name cell))
                (error 'unavailable-function :name (fcell-name cell)
                                             :version (fcell-version cell))))))

(defstruct (gtype-cell (:constructor make-gtype-cell (name get-type)))
  name get-type (value nil))

(defun gtype-cell-gtype (cell)
  (or (gtype-cell-value cell)
      (setf (gtype-cell-value cell)
            (gtype-from-name (gtype-cell-name cell) (gtype-cell-get-type cell)))))

;;; Enums and flags

(defstruct enum-info kind (by-keyword (make-hash-table)) (by-value (make-hash-table)) members)

(defun register-genum (name kind members &key gtype-name get-type)
  "Record NAME's MEMBERS, an alist of (KEYWORD . VALUE). With GTYPE-NAME,
also register GValue conversion for its GType."
  (let ((info (make-enum-info :kind kind :members members)))
    (loop for (key . value) in members
          do (setf (gethash key (enum-info-by-keyword info)) value)
             (unless (gethash value (enum-info-by-value info))
               (setf (gethash value (enum-info-by-value info)) key)))
    (setf (get name 'enum-info) info)
    ;; A missing _get_type (an older library) only loses GValue conversion.
    (when (and get-type (cffi:foreign-symbol-pointer get-type))
      (let ((gtype (gtype-from-name gtype-name get-type)))
        (when gtype
          (register-enum-converter gtype
                                   (lambda (v) (enum-keyword name v))
                                   (lambda (v) (enum-value name v))))))
    name))

(defun enum-info (name)
  (or (get name 'enum-info) (error "gtk4: ~s is not an enum or flags type" name)))

(defun enum-value (name value)
  "The integer for VALUE in enum or flags type NAME. VALUE may be an integer,
a keyword, or for flags a list of keywords."
  (let ((info (enum-info name)))
    (flet ((one (k)
             (cond ((integerp k) k)
                   ((gethash k (enum-info-by-keyword info)))
                   (t (error "gtk4: ~s is not a member of ~s; expected one of ~{~s~^ ~}"
                             k name (mapcar #'car (enum-info-members info)))))))
      (if (listp value)
          (reduce #'logior (mapcar #'one value) :initial-value 0)
          (one value)))))

(defun enum-keyword (name integer)
  "The keyword for INTEGER in enum NAME, or for flags the list of keywords
whose bits are set. Unknown values are returned as integers."
  (let ((info (enum-info name)))
    (if (eq (enum-info-kind info) :flags)
        (loop for (key . value) in (enum-info-members info)
              when (and (/= value 0) (= value (logand value integer))) collect key)
        (or (gethash integer (enum-info-by-value info)) integer))))

(defmacro define-genum (name (&key (kind :enum) gtype-name get-type documentation) &body members)
  `(progn
     (register-genum ',name ,kind ',members :gtype-name ,gtype-name :get-type ,get-type)
     ,@(when documentation `((setf (documentation ',name 'type) ,documentation)))
     ',name))

(defmacro define-gconstant (name value &optional documentation)
  `(defparameter ,name ,value ,@(when documentation (list documentation))))

;;; Classes and records

(defmacro define-gclass (name superclasses (&key gtype-name get-type documentation))
  `(defclass ,name ,superclasses ()
     (:metaclass gobject-class)
     ,@(when gtype-name `((:gtype-name ,gtype-name)))
     ,@(when get-type `((:get-type ,get-type)))
     ,@(when documentation `((:documentation ,documentation)))))

(defmacro define-grecord (name (&key gtype-name documentation))
  "A boxed type: values arrive as BOXED proxies of class NAME."
  `(progn
     (defclass ,name (boxed) ()
       ,@(when documentation `((:documentation ,documentation))))
     ,@(when gtype-name
         `((setf (gethash ,gtype-name *boxed-classes*) (find-class ',name))))
     ',name))

;;; Properties

(defmacro define-gproperty (name property-name (&key readable writable documentation))
  `(progn
     ,@(when readable
         `((defun ,name (object)
             ,@(when documentation (list documentation))
             (property object ,property-name))))
     ,@(when writable
         `((defun (setf ,name) (value object)
             (setf (property object ,property-name) value))))
     ',name))

;;; Functions

(defun spec-kind (spec) (if (consp spec) (first spec) spec))

(defun spec-foreign-type (spec)
  (case (spec-kind spec)
    (:gtype 'gtype)
    ((:string :strv :object :boxed :record :pointer :callback :array :byte-array
      :glist :gslist :ghash :gptrarray :gvalue)
     :pointer)
    (:enum :int)
    (:flags :uint)
    (t spec)))

(defun strv-to-foreign (list)
  "A freshly allocated NULL-terminated char** for LIST; free with FREE-STRV."
  (let ((v (cffi:foreign-alloc :pointer :count (1+ (length list)))))
    (loop for s in list for i from 0
          do (setf (cffi:mem-aref v :pointer i) (cffi:foreign-string-alloc s)))
    (setf (cffi:mem-aref v :pointer (length list)) (cffi:null-pointer))
    v))

(defun strv-to-gmalloc (list)
  "A NULL-terminated char** allocated with g_malloc, for C to free with g_strfreev."
  (if (null list)
      (cffi:null-pointer)
      (let ((v (cffi:foreign-funcall "g_malloc0" :size (* (1+ (length list))
                                                          (cffi:foreign-type-size :pointer))
                                     :pointer)))
        (loop for s in list for i from 0
              do (setf (cffi:mem-aref v :pointer i)
                       (cffi:foreign-funcall "g_strdup" :string s :pointer)))
        v)))

(defun free-strv (v)
  (loop for i from 0
        for p = (cffi:mem-aref v :pointer i)
        until (cffi:null-pointer-p p)
        do (cffi:foreign-string-free p))
  (cffi:foreign-free v))

(defun strv-from-foreign (v transfer)
  (unless (cffi:null-pointer-p v)
    (prog1 (loop for i from 0
                 for p = (cffi:mem-aref v :pointer i)
                 until (cffi:null-pointer-p p)
                 collect (cffi:foreign-string-to-lisp p))
      (case transfer
        (:full (cffi:foreign-funcall "g_strfreev" :pointer v :void))
        (:container (%g-free v))))))

(defun string-from-foreign (p transfer)
  (unless (cffi:null-pointer-p p)
    (prog1 (cffi:foreign-string-to-lisp p)
      (when (eq transfer :full) (%g-free p)))))

(defun pointer-or-nil (p)
  (unless (cffi:null-pointer-p p) p))

(defun convert-from-foreign (form spec transfer)
  "Code converting the foreign value FORM (a return value, out parameter or
callback argument) to Lisp."
  (ecase (spec-kind spec)
    ((:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
      :short :ushort :int :uint :long :ulong :size :ssize :intptr :uintptr
      :float :double :gtype)
     form)
    (:string `(string-from-foreign ,form ,transfer))
    (:strv `(strv-from-foreign ,form ,transfer))
    (:object `(wrap-object ,form :transfer ,(if (eq transfer :full) :full :none)))
    (:boxed (destructuring-bind (gtype-name get-type &optional struct) (rest spec)
              (declare (ignore struct))
              `(wrap-boxed ,form (gtype-cell-gtype (load-time-value (make-gtype-cell ,gtype-name ,get-type)))
                           :transfer ,(if (eq transfer :full) :full :none))))
    ((:record :pointer) `(pointer-or-nil ,form))
    (:byte-array `(byte-array-from-foreign ,form ,transfer))
    ((:glist :gslist)
     `(glist-from-foreign ,form ,(element-reader (second spec) (if (eq transfer :full) :full :none))
                          ,(and (member transfer '(:full :container)) t)
                          ,(eq (spec-kind spec) :gslist)))
    (:gptrarray
     `(ptr-array-from-foreign ,form ,(element-reader (second spec) :none)
                              ,(and (member transfer '(:full :container)) t)))
    (:ghash
     `(hash-table-from-foreign ,form ,(element-reader (second spec) :none)
                               ,(element-reader (third spec) :none)
                               ,(and (member transfer '(:full :container)) t)))
    ((:enum :flags) `(enum-keyword ',(second spec) ,form))))

(defun convert-to-foreign (form spec transfer)
  "Code converting the Lisp value FORM to a foreign value that C keeps after
the call returns (a callback's return value)."
  (ecase (spec-kind spec)
    (:boolean `(and ,form t))
    ((:int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
      :short :ushort :int :uint :long :ulong :size :ssize :intptr :uintptr :gtype)
     `(or ,form 0))
    (:float `(float (or ,form 0) 1f0))
    (:double `(float (or ,form 0) 1d0))
    (:string `(let ((s ,form))
                (if s (cffi:foreign-funcall "g_strdup" :string s :pointer) (cffi:null-pointer))))
    (:strv `(strv-to-gmalloc ,form))
    (:object (if (eq transfer :full)
                 `(let ((p (object-pointer ,form)))
                    (unless (cffi:null-pointer-p p) (%g-object-ref p))
                    p)
                 `(object-pointer ,form)))
    (:boxed (destructuring-bind (gtype-name get-type &optional struct) (rest spec)
              (declare (ignore struct))
              (if (eq transfer :full)
                  `(let ((p (object-pointer ,form)))
                     (if (cffi:null-pointer-p p)
                         p
                         (%g-boxed-copy (gtype-cell-gtype
                                         (load-time-value (make-gtype-cell ,gtype-name ,get-type)))
                                        p)))
                  `(object-pointer ,form))))
    ((:record :pointer) `(object-pointer ,form))
    ((:glist :gslist)
     `(glist-to-foreign ,form ,(element-writer (second spec) transfer) ,(eq (spec-kind spec) :gslist)))
    ((:enum :flags) `(enum-value ',(second spec) ,form))))

(defun foreign-zero (spec)
  "The foreign value a callback returns when its Lisp function fails."
  (case (spec-foreign-type spec)
    (:void nil)
    (:boolean nil)
    (:pointer '(cffi:null-pointer))
    (:float 0f0)
    (:double 0d0)
    (t 0)))

;;; C arrays
;;;
;;; (:array ELEMENT &key length zero-terminated fixed-size caller-allocates)
;;; LENGTH names the argument holding the element count. guint8 arrays are
;;; Lisp octet vectors; all others are lists. Input accepts any sequence.

(defun g-malloc0 (bytes)
  (cffi:foreign-funcall "g_malloc0" :size (max bytes 1) :pointer))

(defun array-to-foreign (sequence ftype converter &key zero-terminated fixed-size)
  "A g_malloc'd C array holding SEQUENCE's elements, each passed through
CONVERTER. NIL gives NULL."
  (if (null sequence)
      (cffi:null-pointer)
      (let* ((n (length sequence))
             (slots (or fixed-size n))
             (p (g-malloc0 (* (+ slots (if zero-terminated 1 0)) (cffi:foreign-type-size ftype))))
             (i 0))
        (map nil (lambda (e)
                   (when (< i slots)
                     (setf (cffi:mem-aref p ftype i) (funcall converter e)))
                   (incf i))
             sequence)
        p)))

(defun zero-element-p (p ftype i)
  (let ((v (cffi:mem-aref p ftype i)))
    (if (cffi:pointerp v) (cffi:null-pointer-p v) (eql v 0))))

(defun array-from-foreign (p ftype converter &key count zero-terminated octets free-container)
  "Lisp elements of the C array P: an octet vector when OCTETS, else a list of
each element passed through CONVERTER. COUNT gives the length; otherwise the
array is ZERO-TERMINATED. With FREE-CONTAINER the array itself is g_free'd."
  (if (cffi:null-pointer-p p)
      (if octets (make-array 0 :element-type '(unsigned-byte 8)) nil)
      (let* ((n (or count
                    (and zero-terminated
                         (loop for i from 0 until (zero-element-p p ftype i) finally (return i)))
                    (error "gtk4: C array without a length or terminator")))
             (result (if octets
                         (let ((v (make-array n :element-type '(unsigned-byte 8))))
                           (dotimes (i n v) (setf (aref v i) (cffi:mem-aref p :uint8 i))))
                         (loop for i below n collect (funcall converter (cffi:mem-aref p ftype i))))))
        (when free-container (%g-free p))
        result)))

(defun free-foreign-array (p ftype free-elements count zero-terminated)
  "Free a C array made by ARRAY-TO-FOREIGN, g_free'ing each element first
when FREE-ELEMENTS (strings duplicated for the call)."
  (unless (cffi:null-pointer-p p)
    (when free-elements
      (loop for i from 0
            while (if count (< i count) (not (and zero-terminated (zero-element-p p ftype i))))
            do (%g-free (cffi:mem-aref p :pointer i))))
    (%g-free p)))

;;; GByteArray

(cffi:defcstruct gbyte-array (data :pointer) (len :uint))

(defun byte-array-to-foreign (octets)
  (if (null octets)
      (cffi:null-pointer)
      (let ((ba (cffi:foreign-funcall "g_byte_array_sized_new" :uint (length octets) :pointer)))
        (cffi:with-pointer-to-vector-data (data (coerce octets '(simple-array (unsigned-byte 8) (*))))
          (cffi:foreign-funcall "g_byte_array_append" :pointer ba :pointer data
                                :uint (length octets) :pointer))
        ba)))

(defun byte-array-from-foreign (ba transfer)
  (unless (cffi:null-pointer-p ba)
    (let* ((n (cffi:foreign-slot-value ba '(:struct gbyte-array) 'len))
           (data (cffi:foreign-slot-value ba '(:struct gbyte-array) 'data))
           (v (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref v i) (cffi:mem-aref data :uint8 i)))
      (when (eq transfer :full)
        (cffi:foreign-funcall "g_byte_array_unref" :pointer ba :void))
      v)))

(defun byte-array-unref (ba)
  (unless (cffi:null-pointer-p ba)
    (cffi:foreign-funcall "g_byte_array_unref" :pointer ba :void)))

(defun array-read-form (spec ptr-form transfer count-form)
  "Code converting the C array PTR-FORM described by SPEC to Lisp."
  (destructuring-bind (element &key zero-terminated &allow-other-keys) (rest spec)
    (declare (ignorable zero-terminated))
    (when (struct-spec-name element)
      (let ((e (gensym "E")))
        (return-from array-read-form
          `(struct-array-from-foreign ,ptr-form ,(struct-size-form element) ,count-form
                                      (lambda (,e) ,(struct-copy-form e element))
                                      ,(and (member transfer '(:full :container)) t)))))
    (let ((e (gensym "E")))
      `(array-from-foreign ,ptr-form ',(spec-foreign-type element)
                           (lambda (,e) ,(convert-from-foreign e element (if (eq transfer :full) :full :none)))
                           :count ,count-form
                           :zero-terminated ,zero-terminated
                           :octets ,(eq element :uint8)
                           :free-container ,(and (member transfer '(:full :container)) t)))))

(defun array-write-form (spec var)
  "Code making a C array from the Lisp sequence VAR."
  (destructuring-bind (element &key zero-terminated fixed-size &allow-other-keys) (rest spec)
    (when (struct-spec-name element)
      (return-from array-write-form
        `(struct-array-to-foreign ,var ,(struct-size-form element) ,zero-terminated ,fixed-size)))
    (let ((e (gensym "E")))
      `(array-to-foreign ,var ',(spec-foreign-type element)
                         (lambda (,e) ,(convert-to-foreign e element :none))
                         :zero-terminated ,zero-terminated :fixed-size ,fixed-size))))

;;; GLib containers
;;;
;;; (:glist ELEMENT) (:gslist ELEMENT) -> lists; (:gptrarray ELEMENT) -> list;
;;; (:ghash KEY VALUE) -> EQUAL hash table (input: hash table or alist).
;;; Elements are stored as pointers: pointer-like values directly, integers
;;; as GINT_TO_POINTER.

(defparameter *integer-element-kinds*
  '(:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
    :int :uint :long :ulong :size :ssize :intptr :uintptr :gtype))

(defun signed-address (p)
  (let ((a (cffi:pointer-address p)))
    (if (>= a (expt 2 63)) (- a (expt 2 64)) a)))

(defun element-from-pointer-form (form element transfer)
  (case (spec-kind element)
    (:boolean `(/= 0 (cffi:pointer-address ,form)))
    ((:enum :flags) `(enum-keyword ',(second element) (signed-address ,form)))
    (t (if (member (spec-kind element) *integer-element-kinds*)
           `(signed-address ,form)
           (convert-from-foreign form element transfer)))))

(defun element-to-pointer-form (form element transfer)
  (case (spec-kind element)
    (:boolean `(cffi:make-pointer (if ,form 1 0)))
    ((:enum :flags) `(cffi:make-pointer (ldb (byte 64 0) (enum-value ',(second element) ,form))))
    (t (if (member (spec-kind element) *integer-element-kinds*)
           `(cffi:make-pointer (ldb (byte 64 0) ,form))
           (convert-to-foreign form element transfer)))))

(defun element-reader (element transfer)
  (let ((p (gensym "P")))
    `(lambda (,p) ,(element-from-pointer-form p element transfer))))

(defun element-writer (element transfer)
  (let ((e (gensym "E")))
    `(lambda (,e) ,(element-to-pointer-form e element transfer))))

(defun glist-from-foreign (list reader free-list single)
  "A Lisp list of LIST's elements through READER; g_(s)list_free when FREE-LIST."
  (prog1 (loop for node = list then (cffi:mem-aref node :pointer 1)
               until (cffi:null-pointer-p node)
               collect (funcall reader (cffi:mem-aref node :pointer 0)))
    (when (and free-list (not (cffi:null-pointer-p list)))
      (if single
          (cffi:foreign-funcall "g_slist_free" :pointer list :void)
          (cffi:foreign-funcall "g_list_free" :pointer list :void)))))

(defun glist-to-foreign (sequence writer single)
  (let ((list (cffi:null-pointer)))
    (map nil (lambda (e)
               (setf list (if single
                              (cffi:foreign-funcall "g_slist_prepend" :pointer list
                                                    :pointer (funcall writer e) :pointer)
                              (cffi:foreign-funcall "g_list_prepend" :pointer list
                                                    :pointer (funcall writer e) :pointer))))
         sequence)
    (if single
        (cffi:foreign-funcall "g_slist_reverse" :pointer list :pointer)
        (cffi:foreign-funcall "g_list_reverse" :pointer list :pointer))))

(defun free-glist (list single free-elements)
  "Free a list made by GLIST-TO-FOREIGN for one call, g_free'ing duplicated
string elements when FREE-ELEMENTS."
  (unless (cffi:null-pointer-p list)
    (when free-elements
      (loop for node = list then (cffi:mem-aref node :pointer 1)
            until (cffi:null-pointer-p node)
            do (%g-free (cffi:mem-aref node :pointer 0))))
    (if single
        (cffi:foreign-funcall "g_slist_free" :pointer list :void)
        (cffi:foreign-funcall "g_list_free" :pointer list :void))))

(cffi:defcstruct gptr-array (pdata :pointer) (len :uint))

(defun ptr-array-from-foreign (array reader unref)
  (unless (cffi:null-pointer-p array)
    (let ((data (cffi:foreign-slot-value array '(:struct gptr-array) 'pdata))
          (n (cffi:foreign-slot-value array '(:struct gptr-array) 'len)))
      (prog1 (loop for i below n collect (funcall reader (cffi:mem-aref data :pointer i)))
        (when unref (cffi:foreign-funcall "g_ptr_array_unref" :pointer array :void))))))

(defun hash-table-from-foreign (table key-reader value-reader unref)
  (unless (cffi:null-pointer-p table)
    (let ((result (make-hash-table :test 'equal)))
      (cffi:with-foreign-objects ((iter :uint8 64) (key :pointer) (value :pointer))
        (cffi:foreign-funcall "g_hash_table_iter_init" :pointer iter :pointer table :void)
        (loop while (cffi:foreign-funcall "g_hash_table_iter_next" :pointer iter
                                          :pointer key :pointer value :boolean)
              do (setf (gethash (funcall key-reader (cffi:mem-ref key :pointer)) result)
                       (funcall value-reader (cffi:mem-ref value :pointer)))))
      (when unref (cffi:foreign-funcall "g_hash_table_unref" :pointer table :void))
      result)))

(defun hash-table-to-foreign (table key-writer value-writer string-keys free-keys free-values)
  "A new GHashTable holding TABLE (a hash table or alist). Strings keys use
g_str_hash; duplicated strings are freed with the table."
  (if (null table)
      (cffi:null-pointer)
      (let* ((free (cffi:foreign-symbol-pointer "g_free"))
             (ht (cffi:foreign-funcall "g_hash_table_new_full"
                                       :pointer (cffi:foreign-symbol-pointer
                                                 (if string-keys "g_str_hash" "g_direct_hash"))
                                       :pointer (cffi:foreign-symbol-pointer
                                                 (if string-keys "g_str_equal" "g_direct_equal"))
                                       :pointer (if free-keys free (cffi:null-pointer))
                                       :pointer (if free-values free (cffi:null-pointer))
                                       :pointer)))
        (flet ((add (k v)
                 (cffi:foreign-funcall "g_hash_table_insert" :pointer ht
                                       :pointer (funcall key-writer k)
                                       :pointer (funcall value-writer v) :boolean)))
          (if (hash-table-p table)
              (maphash #'add table)
              (loop for (k . v) in table do (add k v))))
        ht)))

(defun hash-table-unref (ht)
  (unless (cffi:null-pointer-p ht)
    (cffi:foreign-funcall "g_hash_table_unref" :pointer ht :void)))

;;; Structs
;;;
;;; A struct spec is (:boxed GTYPE-NAME GET-TYPE STRUCT) or (:record STRUCT)
;;; where STRUCT names a layout made by DEFINE-GSTRUCT. Where such a spec
;;; is a caller-allocated out argument or an array element, the struct is
;;; stored inline and copied into a proxy.

(defun struct-spec-name (spec)
  "The layout name of a struct spec, or NIL if the struct's layout is unknown."
  (case (spec-kind spec)
    (:boxed (fourth spec))
    (:record (second spec))))

(defun struct-type (name)
  "The CFFI type designator for layout NAME: (:struct NAME) or (:union NAME)."
  (list (if (eq (get name 'gstruct-kind) :union) :union :struct) name))

(defun struct-size-form (spec)
  `(cffi:foreign-type-size ',(struct-type (struct-spec-name spec))))

(defmacro define-gstruct (name (&key union gtype-name documentation) &body fields)
  "Define the C layout NAME from FIELDS, each (SLOT CFFI-TYPE &key count). For
a struct without a GType, also define a RECORD proxy class named NAME."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'gstruct-kind) ,(if union :union :struct)))
     (,(if union 'cffi:defcunion 'cffi:defcstruct) ,name ,@fields)
     ,@(unless gtype-name
         `((defclass ,name (record) ()
             ,@(when documentation `((:documentation ,documentation))))))
     ',name))

(defun struct-copy-form (pointer-form spec)
  "Code making an owned proxy from the struct at POINTER-FORM."
  (if (eq (spec-kind spec) :boxed)
      `(wrap-boxed ,pointer-form
                   (gtype-cell-gtype (load-time-value (make-gtype-cell ,(second spec) ,(third spec))))
                   :transfer :none)
      `(copy-record ,pointer-form ',(second spec) ,(struct-size-form spec))))

(defun field-read-form (pointer struct slot spec bits &optional inline)
  (when inline
    ;; An embedded struct: return an owned copy.
    (return-from field-read-form
      (struct-copy-form `(cffi:foreign-slot-pointer ,pointer ',(struct-type struct) ',slot) spec)))
  (let ((raw `(cffi:foreign-slot-value ,pointer ',(struct-type struct) ',slot)))
    (when bits
      (setf raw `(ldb (byte ,(first bits) ,(second bits)) ,raw)))
    (case (spec-kind spec)
      (:boolean (if bits `(/= 0 ,raw) raw))
      ((:enum :flags) `(enum-keyword ',(second spec) ,raw))
      (t (convert-from-foreign raw spec :none)))))

(defun field-write-form (pointer struct slot spec bits value &optional inline)
  (when inline
    (return-from field-write-form
      `(cffi:foreign-funcall "memcpy"
                             :pointer (cffi:foreign-slot-pointer ,pointer ',(struct-type struct) ',slot)
                             :pointer (object-pointer ,value)
                             :size ,(struct-size-form spec) :pointer)))
  (let ((place `(cffi:foreign-slot-value ,pointer ',(struct-type struct) ',slot))
        (v (case (spec-kind spec)
             (:boolean (if bits `(if ,value 1 0) `(and ,value t)))
             (:float `(float ,value 1f0))
             (:double `(float ,value 1d0))
             ((:enum :flags) `(enum-value ',(second spec) ,value))
             (t value))))
    (if bits
        `(setf ,place (dpb ,v (byte ,(first bits) ,(second bits)) ,place))
        `(setf ,place ,v))))

(defmacro define-gfield (name struct slot spec &key writable bits inline documentation)
  "Define NAME reading SLOT of STRUCT (and (SETF NAME) when WRITABLE).
BITS is (SIZE POSITION) for a bitfield packed into SLOT. INLINE means SLOT
embeds a struct described by SPEC: reading copies it, writing copies into it."
  `(progn
     (defun ,name (object)
       ,@(when documentation (list documentation))
       ,(field-read-form '(object-pointer object) struct slot spec bits inline))
     ,@(when writable
         `((defun (setf ,name) (value object)
             ,(field-write-form '(object-pointer object) struct slot spec bits 'value inline)
             value)))
     ',name))

(defmacro define-gstruct-constructor (name spec (&rest fields) &key documentation)
  "Define NAME taking a keyword argument per field, each (VAR SLOT FIELD-SPEC
&key bits), and returning a new proxy for the struct SPEC."
  (let ((tmp (gensym "TMP"))
        (struct (struct-spec-name spec))
        (supplied (loop for f in fields collect (gensym (format nil "~a-SUPPLIED-P" (first f))))))
    `(defun ,name (&key ,@(loop for (var) in fields for s in supplied collect `(,var nil ,s)))
       ,@(when documentation (list documentation))
       (cffi:with-foreign-object (,tmp ',(struct-type struct))
         (cffi:foreign-funcall "memset" :pointer ,tmp :int 0 :size ,(struct-size-form spec) :pointer)
         ,@(loop for (var slot field-spec . options) in fields
                 for s in supplied
                 collect `(when ,s
                            ,(field-write-form tmp struct slot field-spec (getf options :bits) var
                                               (getf options :inline))))
         ,(struct-copy-form tmp spec)))))

(defun struct-array-to-foreign (sequence size zero-terminated fixed-size)
  "A g_malloc'd array of SIZE-byte structs copied from SEQUENCE's proxies."
  (if (null sequence)
      (cffi:null-pointer)
      (let* ((n (length sequence))
             (slots (or fixed-size n))
             (p (g-malloc0 (* (+ slots (if zero-terminated 1 0)) size)))
             (i 0))
        (map nil (lambda (e)
                   (when (< i slots)
                     (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer p (* i size))
                                           :pointer (object-pointer e) :size size :pointer))
                   (incf i))
             sequence)
        p)))

(defun struct-array-from-foreign (p size count converter free-container)
  (unless (cffi:null-pointer-p p)
    (unless count (error "gtk4: struct array without a length"))
    (prog1 (loop for i below count collect (funcall converter (cffi:inc-pointer p (* i size))))
      (when free-container (%g-free p)))))

;;; GValue out arguments: read into a Lisp value, then unset.

(defun gvalue-take (gvalue)
  (unwind-protect
       (if (zerop (gvalue-type gvalue)) nil (gvalue-get gvalue))
    (unless (zerop (gvalue-type gvalue)) (%g-value-unset gvalue))))

;;; Callbacks
;;;
;;; Each GIR callback type gets one static trampoline (a CFFI callback named
;;; by the type's symbol). The Lisp function travels through the C
;;; user_data pointer as a handle holding a CALLBACK-ENTRY.

(defstruct (callback-entry (:constructor make-callback-entry (function once)))
  "FUNCTION is a function or a symbol naming one. ONCE means the handle is
freed after the first call (GIR scope \"async\")."
  function once)

(defmacro define-gcallback (name (&key args (return :void) (return-transfer :none) documentation))
  "Define the trampoline for the callback type NAME. ARGS is a list of
(VAR SPEC &key transfer user-data); exactly one argument, marked :user-data,
carries the handle. The Lisp function receives the other arguments in order."
  (declare (ignore documentation))
  (let* ((data (or (first (find-if (lambda (a) (getf (cddr a) :user-data)) args))
                   (error "define-gcallback ~s: no :user-data argument" name)))
         (entry (gensym "ENTRY"))
         (call `(funcall (resolve-handler (callback-entry-function ,entry))
                         ,@(loop for (var spec . options) in args
                                 unless (getf options :user-data)
                                   collect (convert-from-foreign var spec
                                                                 (getf options :transfer :none))))))
    `(progn
       (cffi:defcallback ,name ,(spec-foreign-type return)
           ,(loop for (var spec) in args collect (list var (spec-foreign-type spec)))
         (let ((,entry (handle-value ,data)))
           (unwind-protect
                (with-callback-protection (',name ,(foreign-zero return))
                  ,(if (eq return :void)
                       `(progn ,call nil)
                       (convert-to-foreign call return return-transfer)))
             (when (and ,entry (callback-entry-once ,entry))
               (free-handle ,data)))))
       ',name)))

;;; Each argument wrapper returns BODY wrapped so that PLACE is bound to the
;;; foreign value of the Lisp argument VAR, with any cleanup after BODY.

(defun wrap-in-argument (var spec transfer place body)
  (ecase (spec-kind spec)
    ((:boolean :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
      :short :ushort :int :uint :long :ulong :size :ssize :intptr :uintptr :gtype)
     `(let ((,place ,var)) ,body))
    (:float `(let ((,place (float ,var 1f0))) ,body))
    (:double `(let ((,place (float ,var 1d0))) ,body))
    (:string
     (if (eq transfer :full)
         `(let ((,place (if ,var
                            (cffi:foreign-funcall "g_strdup" :string ,var :pointer)
                            (cffi:null-pointer))))
            ,body)
         `(let ((,place (if ,var (cffi:foreign-string-alloc ,var) (cffi:null-pointer))))
            (unwind-protect ,body
              (unless (cffi:null-pointer-p ,place) (cffi:foreign-string-free ,place))))))
    (:strv
     `(let ((,place (if ,var (strv-to-foreign ,var) (cffi:null-pointer))))
        (unwind-protect ,body
          (unless (cffi:null-pointer-p ,place) (free-strv ,place)))))
    (:object
     (if (eq transfer :full)
         `(let ((,place (object-pointer ,var)))
            (unless (cffi:null-pointer-p ,place) (%g-object-ref ,place))
            ,body)
         `(let ((,place (object-pointer ,var))) ,body)))
    ((:boxed :record :pointer) `(let ((,place (object-pointer ,var))) ,body))
    (:byte-array `(let ((,place (byte-array-to-foreign ,var)))
                    (unwind-protect ,body (byte-array-unref ,place))))
    ((:glist :gslist)
     (let ((single (eq (spec-kind spec) :gslist))
           (element (second spec)))
       (if (eq transfer :none)
           `(let ((,place (glist-to-foreign ,var ,(element-writer element :none) ,single)))
              (unwind-protect ,body
                (free-glist ,place ,single ,(eq element :string))))
           ;; :container or :full: C takes the list (and with :full, the elements).
           `(let ((,place (glist-to-foreign ,var ,(element-writer element transfer) ,single)))
              ,body))))
    (:ghash
     (destructuring-bind (key value) (rest spec)
       `(let ((,place (hash-table-to-foreign ,var ,(element-writer key :none) ,(element-writer value :none)
                                             ,(eq key :string) ,(eq key :string) ,(eq value :string))))
          (unwind-protect ,body (hash-table-unref ,place)))))
    ((:enum :flags) `(let ((,place (enum-value ',(second spec) ,var))) ,body))))

(defun out-initial-value (spec)
  "A zero of the right Lisp type for SPEC's foreign type."
  (case (spec-foreign-type spec)
    (:pointer '(cffi:null-pointer))
    (:double 0d0)
    (:float 0f0)
    (:boolean nil)
    (t 0)))

(defmacro define-gfunction ((name c-name) &key args (return :void) (return-transfer :none)
                                                throws version documentation)
  "Define NAME as a Lisp function calling the C function C-NAME.
ARGS is a list of (VAR SPEC &key direction transfer optional user-data-of
destroy-of length-of):
- :in arguments become parameters in order (trailing ones marked :optional
  become &optional); :out arguments become extra return values.
- A (:callback TYPE SCOPE) argument takes a Lisp function or symbol (or NIL);
  the arguments marked :user-data-of and :destroy-of it are hidden.
- An (:array ...) argument takes or returns a sequence; the argument marked
  :length-of it is hidden. :length-of :return measures the return value."
  (flet ((option (arg key &optional default) (getf (cddr arg) key default))
         (array-option (spec key) (getf (cddr spec) key)))
    (let* ((places (loop for a in args collect (cons (first a) (gensym (string (first a))))))
           (hidden-p (lambda (a) (or (option a :user-data-of) (option a :destroy-of)
                                     (option a :length-of))))
           (ins (remove-if (lambda (a) (or (eq (option a :direction :in) :out)
                                           (funcall hidden-p a)))
                           args))
           (required (remove-if (lambda (a) (option a :optional)) ins))
           (optional (remove-if-not (lambda (a) (option a :optional)) ins))
           (callbacks (remove-if-not (lambda (a) (eq (spec-kind (second a)) :callback)) args))
           (handles (loop for a in callbacks collect (cons (first a) (gensym "HANDLE"))))
           (in-arrays (remove-if-not (lambda (a) (and (eq (spec-kind (second a)) :array)
                                                      (eq (option a :direction :in) :in)))
                                     args))
           (counts (loop for a in in-arrays collect (cons (first a) (gensym "COUNT"))))
           (cell (gensym "CELL"))
           (err (gensym "ERR"))
           (result (gensym "RESULT")))
      (labels ((place (var) (cdr (assoc var places)))
               (arg (var) (find var args :key #'first))
               (length-arg-for (array-var)
                 (find-if (lambda (a) (eq (option a :length-of) array-var)) args))
               (count-form (array-var spec)
                 ;; The element count of an out or return array.
                 (let ((len (array-option spec :length)))
                   (cond ((array-option spec :fixed-size))
                         ((and len (option (arg len) :length-of))
                          `(cffi:mem-ref ,(place len) ',(spec-foreign-type (second (arg len)))))
                         (len len)  ; a visible :in argument: its Lisp value
                         (t (let ((l (length-arg-for array-var)))
                              (and l `(cffi:mem-ref ,(place (first l))
                                                    ',(spec-foreign-type (second l))))))))))
        (let* ((out-reads
                 (loop for a in args
                       for (var spec) = a
                       when (and (eq (option a :direction :in) :out) (not (option a :length-of)))
                         collect (cond
                                   ((eq (spec-kind spec) :gvalue)
                                    `(gvalue-take ,(place var)))
                                   ((and (option a :caller-allocates) (struct-spec-name spec))
                                    (struct-copy-form (place var) spec))
                                   ((and (eq (spec-kind spec) :array) (array-option spec :caller-allocates))
                                    (array-read-form spec (place var) :none (count-form var spec)))
                                   ((eq (spec-kind spec) :array)
                                    (array-read-form spec `(cffi:mem-ref ,(place var) :pointer)
                                                     (option a :transfer :none) (count-form var spec)))
                                   (t (convert-from-foreign
                                       `(cffi:mem-ref ,(place var) ',(spec-foreign-type spec))
                                       spec (option a :transfer :none))))))
               (foreign-call
                 `(cffi:foreign-funcall-pointer
                   (fcell-address ,cell) ()
                   ,@(loop for a in args
                           append (list (if (eq (option a :direction :in) :out)
                                            :pointer
                                            (spec-foreign-type (second a)))
                                        (place (first a))))
                   ,@(when throws (list :pointer err))
                   ,(spec-foreign-type return)))
               (return-form
                 (cond ((eq return :void) nil)
                       ((eq (spec-kind return) :array)
                        (list (array-read-form return result return-transfer
                                               (count-form :return return))))
                       (t (list (convert-from-foreign result return return-transfer)))))
               (body
                 `(let ((,result ,foreign-call))
                    (declare (ignorable ,result))
                    (values ,@return-form ,@out-reads))))
          (when throws
            (setf body `(with-gerror (,err) ,body)))
          ;; Wrap from the last argument outwards so conversions run in argument order.
          (loop for a in (reverse args)
                for (var spec) = a
                for place = (place var)
                do (setf body
                         (cond
                           ;; Caller-allocated GValue or struct: zeroed stack memory.
                           ((and (eq (option a :direction :in) :out)
                                 (or (eq (spec-kind spec) :gvalue)
                                     (and (option a :caller-allocates) (struct-spec-name spec))))
                            (let ((type (if (eq (spec-kind spec) :gvalue)
                                            ''(:struct gvalue)
                                            `',(struct-type (struct-spec-name spec)))))
                              `(cffi:with-foreign-object (,place ,type)
                                 (cffi:foreign-funcall "memset" :pointer ,place :int 0
                                                       :size (cffi:foreign-type-size ,type) :pointer)
                                 ,body)))
                           ;; Caller-allocated out array: we allocate the buffer.
                           ((and (eq (option a :direction :in) :out)
                                 (eq (spec-kind spec) :array)
                                 (array-option spec :caller-allocates))
                            `(let ((,place (g-malloc0 (* ,(count-form var spec)
                                                         ,(cffi:foreign-type-size
                                                           (spec-foreign-type (second spec)))))))
                               (unwind-protect ,body (%g-free ,place))))
                           ((eq (option a :direction :in) :out)
                            `(cffi:with-foreign-object (,place ',(spec-foreign-type spec))
                               (setf (cffi:mem-ref ,place ',(spec-foreign-type spec))
                                     ,(out-initial-value spec))
                               ,body))
                           ((option a :length-of)
                            `(let ((,place ,(cdr (assoc (option a :length-of) counts)))) ,body))
                           ((option a :user-data-of)
                            (let ((h (cdr (assoc (option a :user-data-of) handles))))
                              `(let ((,place (or ,h (cffi:null-pointer)))) ,body)))
                           ((option a :destroy-of)
                            (let ((h (cdr (assoc (option a :destroy-of) handles))))
                              `(let ((,place (if ,h (cffi:callback free-handle-notify) (cffi:null-pointer))))
                                 ,body)))
                           ((eq (spec-kind spec) :callback)
                            (let ((h (cdr (assoc var handles))))
                              `(let ((,place (if ,h (cffi:callback ,(second spec)) (cffi:null-pointer))))
                                 ,body)))
                           ((eq (spec-kind spec) :array)
                            `(let ((,place ,(array-write-form spec var)))
                               (unwind-protect ,body
                                 (free-foreign-array ,place ',(spec-foreign-type (second spec))
                                                     ,(eq (second spec) :string)
                                                     ,(cdr (assoc var counts))
                                                     ,(array-option spec :zero-terminated)))))
                           (t (wrap-in-argument var spec (option a :transfer :none) place body)))))
          ;; In-array element counts, computed before any conversion.
          (loop for (var . count) in (reverse counts)
                do (setf body `(let ((,count (if ,var (length ,var) 0))) ,body)))
          ;; Outermost: create each callback's handle. Scope decides who frees it:
          ;; call -> after the C call returns; async -> the trampoline, after one
          ;; call; notified -> GLib, through the destroy notifier; forever -> nobody.
          (loop for a in (reverse callbacks)
                for (var (nil nil scope)) = a
                for h = (cdr (assoc var handles))
                do (setf body
                         `(let ((,h (and ,var (make-handle (make-callback-entry ,var ,(eq scope :async))))))
                            ,(if (eq scope :call)
                                 `(unwind-protect ,body (when ,h (free-handle ,h)))
                                 body))))
          `(progn
             (defun ,name (,@(mapcar #'first required)
                           ,@(when optional (cons '&optional (mapcar #'first optional))))
               ,@(when documentation (list documentation))
               (let ((,cell (load-time-value (make-fcell ,c-name ,version))))
                 (with-gtk-float-traps ,body)))
             ',name))))))
