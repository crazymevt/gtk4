;;;; overrides.lisp — hand-written additions and corrections to GIR data
;;;;
;;;; Some libraries' GIR files are incomplete: cairo's lists its types but
;;;; almost no functions. An override supplies the missing pieces as a
;;;; compact spec, turned into ordinary model objects before planning, so
;;;; they get the same naming, marshalling, memory management and
;;;; documentation as everything read from GIR.

(in-package #:gtk4.generator)

(defvar *overrides* (make-hash-table :test 'equal)
  "Namespace name -> function called with the parsed GIR-NAMESPACE.")

(defvar *override-urls* (make-hash-table :test 'eq)
  "Model item -> its documentation URL, for items whose library does not use
the docs.gtk.org layout.")

(defmacro define-override (namespace (ns) &body body)
  `(setf (gethash ,namespace *overrides*) (lambda (,ns) ,@body)))

(defun apply-overrides (ns)
  (let ((fn (gethash (gir-namespace-name ns) *overrides*)))
    (when fn (funcall fn ns))
    ns))

;;; Spec syntax
;;;
;;; A type is a GIR type name ("gdouble", "utf8", "Context") or
;;; (:array ELEMENT :length INDEX), INDEX counting parameters from 0.
;;; A parameter is (NAME TYPE &key out caller-allocates nullable transfer).
;;; A return value is TYPE or (TYPE :transfer :full).
;;; A function is (NAME RETURN (PARAMETER...) DOC); NAME has no C prefix.

(defun spec-type (spec)
  (if (and (consp spec) (eq (first spec) :array))
      (make-gir-array :element (spec-type (second spec))
                      :length (getf (cddr spec) :length)
                      :zero-terminated nil)
      (make-gir-type :name spec)))

(defun spec-parameter (spec)
  (destructuring-bind (name type &key out caller-allocates nullable (transfer :none)) spec
    (make-gir-parameter :name name :type (spec-type type)
                        :direction (if out :out :in)
                        :caller-allocates caller-allocates
                        :nullable nullable
                        :transfer transfer)))

(defun spec-function (prefix url-base entry)
  "A GIR-CALLABLE for ENTRY. URL-BASE, when given, is the manual page; the
anchor is the C name with underscores turned into hyphens."
  (destructuring-bind (name return params &optional doc) entry
    (let* ((c-name (concatenate 'string prefix name))
           (return-type (if (consp return) (first return) return))
           (return-transfer (if (consp return) (getf (rest return) :transfer :none) :none))
           (callable (make-gir-callable
                      :kind :function :name name :c-identifier c-name
                      :parameters (mapcar #'spec-parameter params)
                      :return-type (spec-type return-type)
                      :return-transfer return-transfer
                      :doc doc)))
      (when url-base
        (setf (gethash callable *override-urls*)
              (format nil "~a#~a" url-base (substitute #\- #\_ c-name))))
      callable)))

(defun add-functions (ns prefix url-base entries)
  "Append ENTRIES (spec functions) to NS's top-level functions."
  (setf (gir-namespace-functions ns)
        (append (gir-namespace-functions ns)
                (mapcar (lambda (e) (spec-function prefix url-base e)) entries))))

(defun remove-functions (ns &rest names)
  "Drop NS's top-level functions NAMES, to be replaced by correct versions."
  (setf (gir-namespace-functions ns)
        (remove-if (lambda (f) (member (gir-item-name f) names :test #'string=))
                   (gir-namespace-functions ns))))

(defun spec-fields (fields)
  (mapcar (lambda (f)
            (destructuring-bind (name type &key (writable t)) f
              (make-gir-field :name name :type (spec-type type)
                              :readable t :writable writable)))
          fields))

(defun set-record-fields (ns name fields)
  "Give the existing record NAME in NS the FIELDS ((NAME TYPE) ...)."
  (let ((record (find name (gir-namespace-classes ns) :key #'gir-item-name :test #'string=)))
    (unless record (error "override: no record ~a in ~a" name (gir-namespace-name ns)))
    (setf (gir-class-fields record) (spec-fields fields))))

(defun add-record (ns name c-type fields &optional doc)
  "Add a plain record (no GType) NAME to NS."
  (setf (gir-namespace-classes ns)
        (append (gir-namespace-classes ns)
                (list (make-gir-class :kind :record :name name :c-type c-type
                                      :fields (spec-fields fields) :doc doc)))))
