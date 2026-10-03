;;;; build.lisp — widget trees from s-expressions
;;;;
;;;;   (gtk:build
;;;;     (gtk:window :title "Greeter"
;;;;       (gtk:box :orientation :vertical :spacing 6
;;;;         (gtk:entry :id :name :placeholder-text "Your name")
;;;;         (gtk:button :label "Greet" :on-clicked 'greet))))
;;;;
;;;; Each node is (CLASS key value ... child ...). Keys are initargs (GObject
;;;; properties or Lisp slots), except:
;;;;   :id ID              record the object under ID (see GTK:BUILD's values)
;;;;   :on-SIGNAL FUNCTION connect FUNCTION to SIGNAL (:on-clicked, :on-notify)
;;;;   :child-type STRING  how the parent adds this child, as in a .ui file's
;;;;                       <child type="..."> ("titlebar", "start", "end", "overlay")
;;;;   :layout PLIST       properties of the child's layout child, as in a .ui
;;;;                       file's <layout>: (:column 1 :row 0) in a gtk:grid
;;;; A child is a node, or any form returning a widget. Children are added the
;;;; way GtkBuilder adds them, so every container works as in a .ui file.

(in-package #:gtk4)

(defparameter *build-keys* '(:id :child-type :layout))

(defun signal-key-p (key)
  (let ((name (symbol-name key)))
    (and (> (length name) 3) (string= "ON-" name :end2 3))))

(defun node-p (form)
  "True for a build node: a list headed by a GObject class name."
  (and (consp form) (symbolp (first form)) (not (keywordp (first form)))
       (let ((class (find-class (first form) nil)))
         (and class (typep class 'gobject:gobject-class)))))

(defun parse-node (form)
  "FORM's class, initargs (key/form pairs), special options and children."
  (let ((initargs '()) (options '()) (signals '()) (children '()))
    (loop with rest = (rest form)
          while rest
          do (let ((item (pop rest)))
               (if (keywordp item)
                   (let ((value (if rest (pop rest) (error "gtk:build: ~s has no value in ~s" item form))))
                     (cond ((member item *build-keys*) (setf (getf options item) value))
                           ((signal-key-p item)
                            (push (cons (intern (subseq (symbol-name item) 3) :keyword) value) signals))
                           (t (push (cons item value) initargs))))
                   (push item children))))
    (values (first form) (reverse initargs) options (nreverse signals) (nreverse children))))

(defun build-form (form ids builder)
  "Code creating the object FORM describes and returning it."
  (multiple-value-bind (class initargs options signals children) (parse-node form)
    (let ((object (gensym "OBJECT")))
      `(let ((,object (make-instance ',class ,@(loop for (k . v) in initargs append (list k v)))))
         ,@(when (getf options :id)
             `((setf (gethash ,(getf options :id) ,ids) ,object)))
         ,@(loop for (signal . handler) in signals
                 collect `(gobject:connect ,object ,signal ,handler))
         ,@(loop for child in children
                 collect (multiple-value-bind (child-form child-type layout)
                             (if (node-p child)
                                 (multiple-value-bind (c i child-options) (parse-node child)
                                   (declare (ignore c i))
                                   (values (build-form child ids builder)
                                           (getf child-options :child-type)
                                           ;; Keys are literal, values evaluated.
                                           (and (getf child-options :layout)
                                                `(list ,@(getf child-options :layout)))))
                                 (values child nil nil))
                           `(add-child ,object ,child-form ,builder ,child-type ,layout)))
         ,object))))

(defun add-child (parent child builder &optional child-type layout)
  "Add CHILD to PARENT as GtkBuilder does for <child type=CHILD-TYPE>, then
set the LAYOUT properties (a plist) on CHILD's layout child."
  (gobject:call-vfunc parent :add-child builder child child-type)
  (when layout
    (let ((manager (gtk:widget-get-layout-manager parent)))
      (unless manager
        (error "gtk:build: ~s has no layout manager for :layout ~s" parent layout))
      (let ((layout-child (gtk:layout-manager-get-layout-child manager child)))
        (loop for (key value) on layout by #'cddr
              do (setf (gobject:property layout-child key) value)))))
  child)

(defmacro gtk:build (form)
  "Create the object tree FORM describes (see build.lisp for the syntax).
Returns the root object and a hash table from each :id to its object."
  (unless (node-p form)
    (error "gtk:build: ~s does not start with a GObject class name" form))
  (let ((ids (gensym "IDS")) (builder (gensym "BUILDER")))
    `(let ((,ids (make-hash-table :test 'equal))
           (,builder (gtk:builder-new)))
       (values ,(build-form form ids builder) ,ids))))
