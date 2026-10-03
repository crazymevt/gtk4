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
    ((:string :strv :object :boxed :record :pointer) :pointer)
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
  "Code converting the foreign value FORM (a return value or out parameter) to Lisp."
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
    ((:enum :flags) `(enum-keyword ',(second spec) ,form))))

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
    ((:enum :flags) `(let ((,place (enum-value ',(second spec) ,var))) ,body))))

(defun out-initial-value (spec)
  (if (eq (spec-foreign-type spec) :pointer) '(cffi:null-pointer) 0))

(defmacro define-gfunction ((name c-name) &key args (return :void) (return-transfer :none)
                                                throws version documentation)
  "Define NAME as a Lisp function calling the C function C-NAME.
ARGS is a list of (VAR SPEC &key direction transfer optional): :in arguments
become parameters in order (trailing ones marked :optional become &optional);
:out arguments become extra return values after the C return value."
  (let* ((ins (remove :out args :key (lambda (a) (getf (cddr a) :direction :in))))
         (required (remove-if (lambda (a) (getf (cddr a) :optional)) ins))
         (optional (remove-if-not (lambda (a) (getf (cddr a) :optional)) ins))
         (cell (gensym "CELL"))
         (err (gensym "ERR"))
         (result (gensym "RESULT"))
         (places (loop for a in args collect (gensym (string (first a)))))
         (out-reads
           (loop for (var spec . options) in args
                 for place in places
                 when (eq (getf options :direction :in) :out)
                   collect (convert-from-foreign
                            `(cffi:mem-ref ,place ',(spec-foreign-type spec))
                            spec (getf options :transfer :none))))
         (foreign-call
           `(cffi:foreign-funcall-pointer
             (fcell-address ,cell) ()
             ,@(loop for (var spec . options) in args
                     for place in places
                     append (list (if (eq (getf options :direction :in) :out)
                                      :pointer
                                      (spec-foreign-type spec))
                                  place))
             ,@(when throws (list :pointer err))
             ,(spec-foreign-type return)))
         (body
           `(let ((,result ,foreign-call))
              (declare (ignorable ,result))
              (values ,@(unless (eq return :void)
                          (list (convert-from-foreign result return return-transfer)))
                      ,@out-reads))))
    (when throws
      (setf body `(with-gerror (,err) ,body)))
    ;; Wrap from the last argument outwards so conversions run in argument order.
    (loop for (var spec . options) in (reverse args)
          for place in (reverse places)
          for transfer = (getf options :transfer :none)
          do (setf body
                   (if (eq (getf options :direction :in) :out)
                       `(cffi:with-foreign-object (,place ',(spec-foreign-type spec))
                          (setf (cffi:mem-ref ,place ',(spec-foreign-type spec))
                                ,(out-initial-value spec))
                          ,body)
                       (wrap-in-argument var spec transfer place body))))
    `(progn
       (defun ,name (,@(mapcar #'first required)
                     ,@(when optional (cons '&optional (mapcar #'first optional))))
         ,@(when documentation (list documentation))
         (let ((,cell (load-time-value (make-fcell ,c-name ,version))))
           (with-gtk-float-traps ,body)))
       ',name)))
