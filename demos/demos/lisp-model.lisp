;;;; lisp-model.lisp — a GListModel implemented in Lisp
;;;; GIO docs: https://docs.gtk.org/gio/iface.ListModel.html

(in-package #:gtk4-demo)

(defclass squares (gobject:object gio:list-model)
  ((count :initarg :count :reader squares-count))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispDemoSquares")
  (:documentation "The squares of 0 to COUNT-1, computed when GTK asks for them."))

(gobject:define-vfunc (squares :get-item-type) (model)
  (declare (ignore model))
  (gobject:class-gtype 'gobject:lisp-object))

(gobject:define-vfunc (squares :get-n-items) (model)
  (squares-count model))

(gobject:define-vfunc (squares :get-item) (model position)
  (when (< position (squares-count model))
    (gobject:make-lisp-object (cons position (* position position)))))

(define-demo lisp-model
    (:title "List model in Lisp"
     :category "Custom widgets"
     :description "SQUARES implements the GListModel interface in Lisp. It claims a million
items but stores none: GTK asks for the rows it shows, and the model computes them. The
list view is made with gtk:make-list-view from a SETUP and a BIND function.")
  (let ((window (make-demo-frame "List model in Lisp" :width 360 :height 480)))
    (gtk:window-set-child
     window
     (scrolled
      (gtk:make-list-view (make-instance 'squares :count 1000000)
                          :setup (lambda () (margins (make-instance 'gtk:label :xalign 0.0) 4))
                          :bind (lambda (label item)
                                  (gtk:label-set-text label (format nil "~:d² = ~:d" (car item) (cdr item)))))))
    window))
