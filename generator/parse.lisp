;;;; parse.lisp — GIR XML to the model in model.lisp

(in-package #:gtk4.generator)

;;; XML helpers

(defun elements (node)
  (loop for child across (plump:children node)
        when (plump:element-p child) collect child))

(defun kids (node &rest tags)
  (loop for el in (elements node)
        when (member (plump:tag-name el) tags :test #'string=) collect el))

(defun kid (node &rest tags)
  (first (apply #'kids node tags)))

(defun attr (node name)
  (plump:attribute node name))

(defun battr (node name &optional default)
  "Boolean attribute: \"1\" is true, \"0\" false, absent DEFAULT."
  (let ((v (attr node name)))
    (cond ((null v) default)
          ((string= v "1") t)
          ((string= v "0") nil)
          (t default))))

(defun iattr (node name)
  (let ((v (attr node name)))
    (and v (parse-integer v :junk-allowed t))))

(defun kattr (node name)
  "Attribute as a keyword: transfer-ownership=\"full\" => :FULL."
  (let ((v (attr node name)))
    (and v (intern (string-upcase v) :keyword))))

(defun split-list (string)
  (and string (uiop:split-string string :separator ",")))

(defun doc-of (node)
  (let ((d (kid node "doc")))
    (and d (plump:decode-entities (plump:text d)))))

(defun item-args (node)
  (list :name (or (attr node "name") (attr node "glib:name"))
        :doc (doc-of node)
        :version (attr node "version")
        :deprecated (battr node "deprecated")
        :deprecated-version (attr node "deprecated-version")
        :introspectable (battr node "introspectable" t)))

;;; Types

(defun parse-type-element (el)
  (let ((tag (plump:tag-name el)))
    (cond
      ((string= tag "type")
       (make-gir-type :name (attr el "name")
                      :c-type (attr el "c:type")
                      :params (mapcar #'parse-type-element (kids el "type" "array"))))
      ((string= tag "array")
       (make-gir-array :name (attr el "name")
                       :c-type (attr el "c:type")
                       :element (let ((k (kid el "type" "array")))
                                  (and k (parse-type-element k)))
                       :length (iattr el "length")
                       ;; Absent means "the planner decides": GIR's default
                       ;; depends on whether LENGTH or FIXED-SIZE is present.
                       :zero-terminated (battr el "zero-terminated" :default)
                       :fixed-size (iattr el "fixed-size")))
      ((string= tag "varargs") :varargs)
      ((string= tag "callback") (parse-callable el :callback)))))

(defun type-of-node (node)
  (let ((k (kid node "type" "array" "varargs" "callback")))
    (and k (parse-type-element k))))

;;; Callables

(defun parse-parameter (el)
  (make-gir-parameter
   :name (attr el "name")
   :type (type-of-node el)
   :direction (or (kattr el "direction") :in)
   :transfer (kattr el "transfer-ownership")
   :nullable (or (battr el "nullable") (battr el "allow-none"))
   :optional (battr el "optional")
   :caller-allocates (battr el "caller-allocates")
   :closure (iattr el "closure")
   :destroy (iattr el "destroy")
   :scope (kattr el "scope")
   :instance-p (string= (plump:tag-name el) "instance-parameter")
   :doc (doc-of el)))

(defun parse-callable (el kind)
  (let ((ret (kid el "return-value"))
        (params (kid el "parameters")))
    (apply #'make-gir-callable
           :kind kind
           :c-identifier (attr el "c:identifier")
           :parameters (and params
                            (mapcar #'parse-parameter
                                    (kids params "instance-parameter" "parameter")))
           :return-type (and ret (type-of-node ret))
           :return-transfer (and ret (kattr ret "transfer-ownership"))
           :return-nullable (and ret (or (battr ret "nullable") (battr ret "allow-none")))
           :throws (battr el "throws")
           :shadows (attr el "shadows")
           :shadowed-by (attr el "shadowed-by")
           :moved-to (attr el "moved-to")
           :invoker (attr el "invoker")
           :when (kattr el "when")
           :detailed (battr el "detailed")
           :action (battr el "action")
           :no-recurse (battr el "no-recurse")
           :no-hooks (battr el "no-hooks")
           (item-args el))))

(defun parse-callables (node tag kind)
  (mapcar (lambda (el) (parse-callable el kind)) (kids node tag)))

;;; Properties, fields, members, enums, constants, aliases

(defun parse-property (el)
  (apply #'make-gir-property
         :type (type-of-node el)
         :readable (battr el "readable" t)
         :writable (battr el "writable")
         :construct (battr el "construct")
         :construct-only (battr el "construct-only")
         :transfer (kattr el "transfer-ownership")
         :getter (attr el "getter")
         :setter (attr el "setter")
         (item-args el)))

(defun parse-field (el)
  (apply #'make-gir-field
         :type (type-of-node el)
         :readable (battr el "readable" t)
         :writable (battr el "writable")
         :private (battr el "private")
         :bits (iattr el "bits")
         (item-args el)))

(defun parse-member (el)
  (make-gir-member :name (attr el "name")
                   :value (let ((v (attr el "value")))
                            (and v (parse-integer v :junk-allowed t)))
                   :c-identifier (attr el "c:identifier")
                   :nick (attr el "glib:nick")
                   :doc (doc-of el)))

(defun parse-enum (el)
  (apply #'make-gir-enum
         :kind (if (string= (plump:tag-name el) "bitfield") :bitfield :enumeration)
         :c-type (attr el "c:type")
         :glib-type-name (attr el "glib:type-name")
         :get-type (attr el "glib:get-type")
         :error-domain (attr el "glib:error-domain")
         :members (mapcar #'parse-member (kids el "member"))
         :functions (parse-callables el "function" :function)
         (item-args el)))

(defun parse-constant (el)
  (apply #'make-gir-constant
         :value (attr el "value")
         :c-type (attr el "c:type")
         :type (type-of-node el)
         (item-args el)))

(defun parse-alias (el)
  (apply #'make-gir-alias
         :c-type (attr el "c:type")
         :type (type-of-node el)
         (item-args el)))

;;; Classes and friends

(defparameter *class-tags*
  '(("class" . :class) ("interface" . :interface) ("record" . :record)
    ("union" . :union) ("glib:boxed" . :boxed)))

(defun parse-class (el)
  (apply #'make-gir-class
         :kind (cdr (assoc (plump:tag-name el) *class-tags* :test #'string=))
         :c-type (attr el "c:type")
         :parent (attr el "parent")
         :abstract (battr el "abstract")
         :final (battr el "final")
         :fundamental (battr el "glib:fundamental")
         :opaque (battr el "opaque")
         :disguised (battr el "disguised")
         :glib-type-name (attr el "glib:type-name")
         :get-type (attr el "glib:get-type")
         :type-struct (attr el "glib:type-struct")
         :is-gtype-struct-for (attr el "glib:is-gtype-struct-for")
         :ref-func (attr el "glib:ref-func")
         :unref-func (attr el "glib:unref-func")
         :copy-function (attr el "copy-function")
         :free-function (attr el "free-function")
         :implements (mapcar (lambda (i) (attr i "name")) (kids el "implements"))
         :prerequisites (mapcar (lambda (i) (attr i "name")) (kids el "prerequisite"))
         :constructors (parse-callables el "constructor" :constructor)
         :methods (parse-callables el "method" :method)
         :functions (parse-callables el "function" :function)
         :virtual-methods (parse-callables el "virtual-method" :virtual-method)
         :properties (mapcar #'parse-property (kids el "property"))
         :signals (parse-callables el "glib:signal" :signal)
         :fields (parse-fields el)
         (item-args el)))

(defun parse-fields (el)
  "Fields in declaration order. An anonymous <union> or <record> nested in a
record becomes a field whose type is that inline GIR-CLASS, so struct
layouts come out right."
  (loop for child in (kids el "field" "union" "record")
        collect (if (string= (plump:tag-name child) "field")
                    (parse-field child)
                    (make-gir-field :name (or (attr child "name") "anonymous")
                                    :type (parse-class child)
                                    :readable nil
                                    :private t))))

;;; Namespace

(defun parse-repository (root source)
  (let* ((repo (or (kid root "repository")
                   (error "~a: no <repository> element" source)))
         (ns (or (kid repo "namespace")
                 (error "~a: no <namespace> element" source))))
    (make-gir-namespace
     :name (attr ns "name")
     :version (attr ns "version")
     :shared-libraries (split-list (attr ns "shared-library"))
     :c-identifier-prefixes (split-list (attr ns "c:identifier-prefixes"))
     :c-symbol-prefixes (split-list (attr ns "c:symbol-prefixes"))
     :includes (mapcar (lambda (i) (list (attr i "name") (attr i "version")))
                       (kids repo "include"))
     :c-includes (mapcar (lambda (i) (attr i "name")) (kids repo "c:include"))
     :packages (mapcar (lambda (i) (attr i "name")) (kids repo "package"))
     :classes (mapcar #'parse-class
                      (apply #'kids ns (mapcar #'car *class-tags*)))
     :enums (mapcar #'parse-enum (kids ns "enumeration" "bitfield"))
     :callbacks (parse-callables ns "callback" :callback)
     :functions (parse-callables ns "function" :function)
     :constants (mapcar #'parse-constant (kids ns "constant"))
     :aliases (mapcar #'parse-alias (kids ns "alias"))
     :source source)))

(defun parse-xml (input)
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (plump:parse input)))

(defun parse-gir-file (pathname)
  "Parse the .gir file at PATHNAME into a GIR-NAMESPACE."
  (parse-repository (parse-xml (pathname pathname)) (namestring pathname)))

(defun parse-gir-string (string)
  "Parse GIR XML held in STRING into a GIR-NAMESPACE."
  (parse-repository (parse-xml string) "<string>"))
