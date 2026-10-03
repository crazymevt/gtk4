;;;; report.lisp — what a loaded repository contains
;;;;
;;;; The start of the coverage report: for now it counts what was parsed;
;;;; later it will also say what was bound, overridden or skipped.

(in-package #:gtk4.generator)

(defun class-callables (ns)
  (loop for c in (gir-namespace-classes ns)
        append (gir-class-constructors c)
        append (gir-class-methods c)
        append (gir-class-functions c)))

(defun namespace-summary (ns)
  "A plist counting the items in NS."
  (let* ((classes (gir-namespace-classes ns))
         (by-kind (lambda (k) (count k classes :key #'gir-class-kind)))
         (enums (gir-namespace-enums ns))
         (callables (append (gir-namespace-functions ns)
                            (class-callables ns)
                            (loop for e in enums append (gir-enum-functions e)))))
    (list :classes (funcall by-kind :class)
          :interfaces (funcall by-kind :interface)
          :records (+ (funcall by-kind :record) (funcall by-kind :boxed))
          :unions (funcall by-kind :union)
          :enums (count :enumeration enums :key #'gir-enum-kind)
          :bitfields (count :bitfield enums :key #'gir-enum-kind)
          :callables (length callables)
          :not-introspectable (count nil callables :key #'gir-item-introspectable)
          :vfuncs (loop for c in classes sum (length (gir-class-virtual-methods c)))
          :properties (loop for c in classes sum (length (gir-class-properties c)))
          :signals (loop for c in classes sum (length (gir-class-signals c)))
          :callbacks (length (gir-namespace-callbacks ns))
          :constants (length (gir-namespace-constants ns)))))

(defparameter *summary-columns*
  '((:classes "Classes") (:interfaces "Ifaces") (:records "Records") (:unions "Unions")
    (:enums "Enums") (:bitfields "Flags") (:callables "Callables")
    (:not-introspectable "Non-GI") (:vfuncs "Vfuncs") (:properties "Props")
    (:signals "Signals") (:callbacks "Callbks") (:constants "Consts")))

(defun print-summary (table &optional (stream *standard-output*))
  "Print a table of counts for every namespace in TABLE (from LOAD-TARGETS)."
  (let ((keys (sort (alexandria:hash-table-keys table) #'string<))
        (totals (make-list (length *summary-columns*) :initial-element 0)))
    (format stream "~&~16a~{~10@a~}~%" "Namespace" (mapcar #'second *summary-columns*))
    (dolist (key keys)
      (let ((ns (gethash key table)))
        (if (eq ns :missing)
            (format stream "~16a  (missing .gir file)~%" key)
            (let ((row (loop for (k) in *summary-columns*
                             collect (getf (namespace-summary ns) k))))
              (setf totals (mapcar #'+ totals row))
              (format stream "~16a~{~10d~}~%" key row)))))
    (format stream "~16a~{~10d~}~%" "Total" totals)
    (values)))
