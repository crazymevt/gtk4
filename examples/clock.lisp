;;;; clock.lisp — custom widgets: a Lisp GType, snapshot, a property, a template
;;;;
;;;; CLOCK-FACE is a widget defined in Lisp. It registers its own GType
;;;; ("LispClockFace"), draws itself in the :snapshot virtual function, says
;;;; how big it wants to be in :measure, and has a GObject property,
;;;; show-seconds, stored in an ordinary slot.
;;;;
;;;; CLOCK-PANEL is a composite widget: its children come from a GtkBuilder
;;;; template, which uses CLOCK-FACE like any GTK class and connects a
;;;; button to a Lisp function.
;;;;
;;;; GTK documentation:
;;;;   https://docs.gtk.org/gtk4/vfunc.Widget.snapshot.html
;;;;   https://docs.gtk.org/gtk4/vfunc.Widget.measure.html
;;;;   https://docs.gtk.org/gtk4/class.Widget.html#building-composite-widgets-from-template-xml
;;;;
;;;; Run with:  make example NAME=clock   (QUIT_AFTER=3 to close automatically)
;;;;
;;;; Live editing: start it from a script that also starts a Swank or Slynk
;;;; server (see the manual's "Custom widgets" chapter), then evaluate a new
;;;; (gobject:define-vfunc (clock-face :snapshot) ...) from your editor. The
;;;; running clock draws with the new definition on its next tick.

(defpackage #:gtk4-examples.clock
  (:use #:cl)
  (:export #:main #:clock-face #:clock-panel))

(in-package #:gtk4-examples.clock)

;;; The clock face

(defclass clock-face (gtk:widget)
  ((show-seconds :initform t :accessor show-seconds
                 :property (:boolean :nick "Show seconds" :blurb "Whether to draw the second hand"))
   (last-drawn :initform nil :accessor last-drawn))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispClockFace"))

(gobject:define-vfunc (clock-face :measure) (widget orientation for-size)
  (declare (ignore widget orientation for-size))
  ;; Minimum and natural size, in both directions; no baselines.
  (values 120 240 -1 -1))

(defun hand (cr angle length width)
  "Draw a clock hand from the center at ANGLE (radians, 0 = twelve o'clock)."
  (cairo:set-line-width cr width)
  (cairo:move-to cr 0 0)
  (cairo:line-to cr (* length (sin angle)) (- (* length (cos angle))))
  (cairo:stroke cr))

(gobject:define-vfunc (clock-face :snapshot) (widget snapshot)
  (let* ((width (gtk:widget-get-width widget))
         (height (gtk:widget-get-height widget))
         (radius (* 0.45 (min width height)))
         (cr (gtk:snapshot-append-cairo
              snapshot (graphene:make-rect :origin (graphene:make-point :x 0.0 :y 0.0)
                                           :size (graphene:make-size :width (float width)
                                                                     :height (float height))))))
    (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
      (cairo:translate cr (/ width 2) (/ height 2))
      ;; Face and hour marks.
      (cairo:arc cr 0 0 radius 0 (* 2 pi))
      (cairo:set-source-rgb cr 0.98 0.97 0.94)
      (cairo:fill-preserve cr)
      (cairo:set-source-rgb cr 0.2 0.2 0.25)
      (cairo:set-line-width cr (* 0.03 radius))
      (cairo:stroke cr)
      (dotimes (i 12)
        (let ((a (* i (/ pi 6))))
          (cairo:move-to cr (* 0.85 radius (sin a)) (* 0.85 radius (cos a)))
          (cairo:line-to cr (* 0.95 radius (sin a)) (* 0.95 radius (cos a)))
          (cairo:stroke cr)))
      ;; Hands.
      (cairo:set-line-cap cr :round)
      (hand cr (* (+ (mod h 12) (/ m 60)) (/ pi 6)) (* 0.5 radius) (* 0.07 radius))
      (hand cr (* (+ m (/ s 60)) (/ pi 30)) (* 0.75 radius) (* 0.05 radius))
      (when (show-seconds widget)
        (cairo:set-source-rgb cr 0.85 0.2 0.15)
        (hand cr (* s (/ pi 30)) (* 0.85 radius) (* 0.02 radius))))))

(defun tick (clock frame-clock)
  "Called before every frame while the clock is on screen: redraw when the
second changes. Uses its argument rather than capturing the widget, so the
callback does not keep the widget alive."
  (declare (ignore frame-clock))
  (let ((now (get-universal-time)))
    (unless (eql now (last-drawn clock))
      (setf (last-drawn clock) now)
      (gtk:widget-queue-draw clock)))
  t)                                    ; keep ticking

(defmethod initialize-instance :after ((clock clock-face) &key)
  (gtk:widget-add-tick-callback clock 'tick))

;;; A composite widget built from a template

(defclass clock-panel (gtk:box)
  ((clock :template-child t :reader panel-clock)
   (toggle :template-child t :reader panel-toggle))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispClockPanel")
  (:template "<interface>
  <template class=\"LispClockPanel\" parent=\"GtkBox\">
    <property name=\"orientation\">vertical</property>
    <property name=\"spacing\">12</property>
    <property name=\"margin-top\">12</property>
    <property name=\"margin-bottom\">12</property>
    <property name=\"margin-start\">12</property>
    <property name=\"margin-end\">12</property>
    <child>
      <object class=\"LispClockFace\" id=\"clock\">
        <property name=\"vexpand\">true</property>
      </object>
    </child>
    <child>
      <object class=\"GtkToggleButton\" id=\"toggle\">
        <property name=\"label\">Hide seconds</property>
        <property name=\"halign\">center</property>
        <signal name=\"toggled\" handler=\"toggle-seconds\" object=\"LispClockPanel\"/>
      </object>
    </child>
  </template>
</interface>"))

(defun toggle-seconds (panel button)
  ;; object="LispClockPanel" makes the panel the first argument.
  (let ((hide (gtk:toggle-button-get-active button)))
    ;; Writing the property's slot emits notify::show-seconds, like
    ;; g_object_set would.
    (setf (show-seconds (panel-clock panel)) (not hide))
    (gtk:button-set-label button (if hide "Show seconds" "Hide seconds"))
    (gtk:widget-queue-draw (panel-clock panel))))

;;; The application

(defun activate (app)
  (let ((window (gtk:application-window-new app)))
    (gtk:window-set-title window "Clock")
    (gtk:window-set-default-size window 320 360)
    (gtk:window-set-child window (make-instance 'clock-panel))
    (gtk:window-present window)))

(defun main (&key quit-after)
  (let ((app (gtk:application-new "org.lisp.gtk4.Clock" '(:default-flags))))
    (gobject:connect app :activate #'activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (gio:application-run app nil)))
