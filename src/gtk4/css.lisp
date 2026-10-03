;;;; css.lisp — CSS from s-expressions
;;;;
;;;;   (gtk:add-css '((".title" :font-size "20pt" :font-weight :bold)
;;;;                  ("button.suggested" :background "#3584e4" :color :white)))

(in-package #:gtk4)

(defun css-value (value)
  (typecase value
    (string value)
    (keyword (string-downcase (symbol-name value)))
    (list (format nil "~{~a~^ ~}" (mapcar #'css-value value)))
    (t (princ-to-string value))))

(defun gtk:css (rules)
  "CSS text for RULES, a list of (SELECTOR property value ...). A property is
a keyword (:font-size) or string; a value is a string, a keyword (:bold), a
number, or a list of these (joined with spaces). A string rule is copied
as is."
  (with-output-to-string (out)
    (dolist (rule rules)
      (if (stringp rule)
          (format out "~a~%" rule)
          (destructuring-bind (selector &rest declarations) rule
            (format out "~a {~%" selector)
            (loop for (property value) on declarations by #'cddr
                  do (format out "  ~a: ~a;~%" (css-value property) (css-value value)))
            (format out "}~%"))))))

(defun gtk:add-css (css &key (display (gdk:display-get-default))
                          (priority gtk:+style-provider-priority-application+))
  "Load CSS (CSS text, or rules as for gtk:css) into a new
gtk:css-provider for every widget on DISPLAY. Returns the provider; remove it
with gtk:style-context-remove-provider-for-display."
  (let ((provider (gtk:css-provider-new)))
    (gtk:css-provider-load-from-string provider (if (stringp css) css (gtk:css css)))
    (gtk:style-context-add-provider-for-display display provider priority)
    provider))
