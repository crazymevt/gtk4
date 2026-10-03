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
    (when get-type
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
    ((:string :strv :object :boxed :record :pointer :callback :array :byte-array) :pointer)
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
    (:boxed (destructuring-bind (gtype-name get-type) (rest spec)
              `(wrap-boxed ,form (gtype-cell-gtype (load-time-value (make-gtype-cell ,gtype-name ,get-type)))
                           :transfer ,(if (eq transfer :full) :full :none))))
    ((:record :pointer) `(pointer-or-nil ,form))
    (:byte-array `(byte-array-from-foreign ,form ,transfer))
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
    (:boxed (destructuring-bind (gtype-name get-type) (rest spec)
              (if (eq transfer :full)
                  `(let ((p (object-pointer ,form)))
                     (if (cffi:null-pointer-p p)
                         p
                         (%g-boxed-copy (gtype-cell-gtype
                                         (load-time-value (make-gtype-cell ,gtype-name ,get-type)))
                                        p)))
                  `(object-pointer ,form))))
    ((:record :pointer) `(object-pointer ,form))
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
    (let ((e (gensym "E")))
      `(array-to-foreign ,var ',(spec-foreign-type element)
                         (lambda (,e) ,(convert-to-foreign e element :none))
                         :zero-terminated ,zero-terminated :fixed-size ,fixed-size))))

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
    ((:enum :flags) `(let ((,place (enum-value ',(second spec) ,var))) ,body))))

(defun out-initial-value (spec)
  (if (eq (spec-foreign-type spec) :pointer) '(cffi:null-pointer) 0))

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
