;;;; gauge.lisp — a widget defined in Lisp: snapshot, measure, a property, a signal
;;;; GTK docs: https://docs.gtk.org/gtk4/vfunc.Widget.snapshot.html

(in-package #:gtk4-demo)

(defclass gauge (gtk:widget)
  ((value :initform 0.3d0 :accessor gauge-value
          :property (:double :min 0 :max 1 :blurb "How full the gauge is, from 0 to 1")))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispDemoGauge")
  (:signals (:full ())))

(gobject:define-vfunc (gauge :measure) (widget orientation for-size)
  (declare (ignore widget orientation for-size))
  (values 100 200 -1 -1))

(gobject:define-vfunc (gauge :snapshot) (widget snapshot)
  (let* ((w (gtk:widget-get-width widget))
         (h (gtk:widget-get-height widget))
         (r (* 0.4 (min w h)))
         (v (gauge-value widget))
         (cr (gtk:snapshot-append-cairo
              snapshot (graphene:make-rect :origin (graphene:make-point :x 0.0 :y 0.0)
                                           :size (graphene:make-size :width (float w) :height (float h))))))
    (cairo:translate cr (/ w 2) (* 0.6 h))
    (cairo:set-line-cap cr :round)
    (cairo:set-line-width cr (* 0.18 r))
    ;; Track, then the filled part, from 7 o'clock to 5 o'clock.
    (cairo:set-source-rgb cr 0.85 0.85 0.88)
    (cairo:arc cr 0 0 r (* 0.75 pi) (* 2.25 pi))
    (cairo:stroke cr)
    (cairo:set-source-rgb cr (+ 0.2 (* 0.7 v)) (- 0.7 (* 0.5 v)) 0.3)
    (cairo:arc cr 0 0 r (* 0.75 pi) (+ (* 0.75 pi) (* v 1.5 pi)))
    (cairo:stroke cr)
    (cairo:select-font-face cr "Sans" :normal :bold)
    (cairo:set-font-size cr (* 0.35 r))
    (let* ((text (format nil "~d%" (round (* 100 v))))
           (e (cairo:text-extents cr text)))
      (cairo:move-to cr (- (/ (cairo:text-extents-width e) 2)) (* 0.15 r))
      (cairo:set-source-rgb cr 0.2 0.2 0.25)
      (cairo:show-text cr text))))

(defmethod (setf gauge-value) :after (value (gauge gauge))
  ;; The slot write already emitted notify::value; redraw, and say when full.
  (gtk:widget-queue-draw gauge)
  (when (>= value 1) (gobject:emit gauge :full)))

(define-demo gauge
    (:title "Drawing widget"
     :category "Custom widgets"
     :description "GAUGE is a widget class defined in Lisp: it registers its own GType, draws
itself in the :snapshot virtual function and sizes itself in :measure. Its VALUE slot is
also a GObject property, bound here to the scale's adjustment with
g_object_bind_property, and it has a Lisp-defined signal, full.")
  (let* ((window (make-demo-frame "Drawing widget" :width 360 :height 360))
         (gauge (make-instance 'gauge :vexpand t))
         (adjustment (gtk:adjustment-new 0.3d0 0d0 1d0 0.01d0 0.1d0 0d0))
         (scale (gtk:scale-new :horizontal adjustment))
         (status (gtk:label-new "Drag the scale")))
    (gobject:object-bind-property adjustment "value" gauge "value" '(:default))
    (gobject:connect gauge :full (lambda (g) (declare (ignore g))
                                   (gtk:label-set-text status "Full!")))
    (gobject:connect gauge "notify::value"
                     (lambda (g p) (declare (ignore p))
                       (when (< (gauge-value g) 1)
                         (gtk:label-set-text status (format nil "~,2f" (gauge-value g))))))
    (gtk:window-set-child window (margins (vbox 12 gauge scale status) 12))
    window))
