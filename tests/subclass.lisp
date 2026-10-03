;;;; subclass.lisp — GTypes defined in Lisp: properties, signals, virtual
;;;; functions, interfaces and composite templates

(in-package #:gtk4-tests)

(define-test subclass :parent gtk4-tests)

;;; Classes under test

(defclass counter (gobject:object)
  ((count :initform 0 :accessor counter-count :property (:int :min 0 :max 100))
   (label :initarg :label :initform "none" :accessor counter-label :property :string)
   (seen-in-constructed :initform nil :accessor seen-in-constructed)
   (lisp-only :initform :plain :accessor lisp-only))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispCounter")
  (:signals (:bumped (:int))
            (:ask (:int) :return :int)))

(gobject:define-vfunc (counter :constructed) (self)
  (setf (seen-in-constructed self) (counter-label self))
  (call-next-vfunc))

(defclass number-list (gobject:object gio:list-model)
  ((items :initarg :items :initform '() :accessor number-list-items))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispNumberList"))

(gobject:define-vfunc (number-list :get-item-type) (self)
  (declare (ignore self))
  (gobject:class-gtype 'counter))

(gobject:define-vfunc (number-list :get-n-items) (self)
  (length (number-list-items self)))

(gobject:define-vfunc (number-list :get-item) (self position)
  (nth position (number-list-items self)))

(defclass padded-number-list (number-list) ()
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispPaddedNumberList"))

(gobject:define-vfunc (padded-number-list :get-n-items) (self)
  (declare (ignore self))
  (+ 1 (call-next-vfunc)))

(defclass picky (gobject:object gio:initable)
  ((how :initarg :how :reader picky-how))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispPicky"))

(gobject:define-vfunc (picky :init) (self cancellable)
  (declare (ignore cancellable))
  (ecase (picky-how self)
    (:ok t)
    (:glib-error (error 'glib:glib-error :domain "g-io-error-quark" :code 14 :message "nope"))
    (:lisp-error (error "plain Lisp failure"))))

;;; GObject-level tests (no display needed)

(define-test registers-a-gtype :parent subclass
  (let ((c (make-instance 'counter)))
    (is string= "TestLispCounter" (rt:gtype-name (rt:instance-gtype (gobject:object-pointer c))))
    (is string= "GObject" (rt:gtype-name (rt:gtype-parent (gobject:class-gtype 'counter))))
    (true (typep c 'gobject:object))))

(define-test properties-live-in-slots :parent subclass
  (let ((c (make-instance 'counter :label "hello"))
        (notified '()))
    (gobject:connect c "notify::count" (lambda (o p) (declare (ignore p))
                                         (push (counter-count o) notified)))
    (setf (counter-count c) 5)
    (is = 5 (gobject:property c :count))
    (setf (gobject:property c :count) 7)
    (is = 7 (counter-count c))
    (is equal '(7 5) notified)
    (is string= "hello" (gobject:property c :label))
    (setf (lisp-only c) :changed)       ; an ordinary slot: no property, no notify
    (is equal '(7 5) notified)))

(define-test properties-bind :parent subclass
  (let ((a (make-instance 'counter)) (b (make-instance 'counter)))
    (gobject:object-bind-property a "count" b "count" '(:default))
    (setf (counter-count a) 42)
    (is = 42 (counter-count b))))

(define-test property-specs-are-real :parent subclass
  (let* ((c (make-instance 'counter))
         (pspec (gobject:object-class-find-property
                 (cffi:foreign-slot-value (gobject:object-pointer c) '(:struct rt::gtype-instance) 'rt::g-class)
                 "count")))
    (false (cffi:null-pointer-p pspec))
    (is string= "count" (gobject:param-spec-get-nick pspec))
    (is = (rt::designator-gtype :int)
        (cffi:foreign-slot-value pspec '(:struct rt::gparam-spec) 'rt::value-type))
    (is = (gobject:class-gtype 'counter)
        (cffi:foreign-slot-value pspec '(:struct rt::gparam-spec) 'rt::owner-type))))

(define-test lisp-signals :parent subclass
  (let ((c (make-instance 'counter)) (got nil))
    (gobject:connect c :bumped (lambda (o n) (declare (ignore o)) (setf got n)))
    (gobject:emit c :bumped 3)
    (is = 3 got)
    (gobject:connect c :ask (lambda (o n) (declare (ignore o)) (* n 10)))
    (is = 40 (gobject:emit c :ask 4))))

(define-test slots-ready-during-construction :parent subclass
  (is string= "early" (seen-in-constructed (make-instance 'counter :label "early")))
  ;; An instance created by C code gets a proxy with the slots' initforms.
  (let* ((p (cffi:foreign-funcall "g_object_new" rt:gtype (gobject:class-gtype 'counter)
                                  :pointer (cffi:null-pointer) :pointer))
         (c (rt:wrap-object p :transfer :full)))
    (is eq 'counter (type-of c))
    (is string= "none" (seen-in-constructed c))
    (is eq :plain (lisp-only c))))

(define-test implements-an-interface :parent subclass
  (let* ((items (list (make-instance 'counter :label "a") (make-instance 'counter :label "b")))
         (model (make-instance 'number-list :items items)))
    (true (typep model 'gio:list-model))
    (is = 2 (gio:list-model-get-n-items model))
    (is eq (second items) (gio:list-model-get-item model 1))
    (is = (gobject:class-gtype 'counter) (gio:list-model-get-item-type model))
    ;; GTK's own models accept it.
    (let ((filtered (gtk:filter-list-model-new model nil)))
      (is = 2 (gio:list-model-get-n-items filtered)))))

(define-test call-next-vfunc-chains :parent subclass
  (let ((model (make-instance 'padded-number-list :items (list (make-instance 'counter)))))
    (is = 2 (gio:list-model-get-n-items model))))

(define-test redefinition-takes-effect :parent subclass
  (let ((model (make-instance 'padded-number-list :items '())))
    (is = 1 (gio:list-model-get-n-items model))
    (unwind-protect
         (progn
           (gobject:define-vfunc (padded-number-list :get-n-items) (self)
             (declare (ignore self))
             (+ 10 (call-next-vfunc)))
           (is = 10 (gio:list-model-get-n-items model))
           (gobject:remove-vfunc 'padded-number-list :get-n-items)
           (is = 0 (gio:list-model-get-n-items model)))
      (gobject:define-vfunc (padded-number-list :get-n-items) (self)
        (declare (ignore self))
        (+ 1 (call-next-vfunc))))))

(define-test errors-become-gerrors :parent subclass
  (let ((rt:*callback-error-handler* (lambda (e where) (declare (ignore e where)))))
    (true (gio:initable-init (make-instance 'picky :how :ok) nil))
    (let ((e (handler-case (progn (gio:initable-init (make-instance 'picky :how :glib-error) nil) nil)
               (glib:glib-error (e) e))))
      (true e)
      (is string= "nope" (glib:glib-error-message e))
      (is = 14 (glib:glib-error-code e)))
    (let ((e (handler-case (progn (gio:initable-init (make-instance 'picky :how :lisp-error) nil) nil)
               (glib:glib-error (e) e))))
      (true e)
      (is string= "gtk4-lisp-error-quark" (glib:glib-error-domain e))
      (true (search "plain Lisp failure" (glib:glib-error-message e))))))

(define-test vfunc-misuse :parent subclass
  ;; A class that shares its parent's GType cannot override.
  (fail (gobject:define-vfunc (tagged-action :activate) (a p) (list a p)))
  (fail (gobject:define-vfunc (counter :no-such-vfunc) (c) c))
  (fail (gobject:define-vfunc (counter :finalize) (c) c)))

(define-test lisp-instances-are-collected :parent subclass
  (setf *finalized* 0)
  (in-fresh-thread
   (lambda ()
     (dotimes (i 50)
       (watch-finalization (make-instance 'counter :label (format nil "c~d" i))))))
  (true (collect-until (lambda () (= *finalized* 50)) :timeout 10)))

;;; GTK widgets (need a display)

(defmacro without-callback-errors (&body body)
  "Run BODY, then fail if any callback reported an error meanwhile (errors
in callbacks are logged rather than signalled)."
  (let ((errors (gensym "ERRORS")))
    `(let ((,errors '()))
       (let ((rt:*callback-error-handler* (lambda (e where) (push (list where (princ-to-string e)) ,errors))))
         ,@body)
       (is equal '() ,errors))))

(defvar *builder-clicks* 0)

(defun note-builder-click (button)
  (declare (ignore button))
  (incf *builder-clicks*))

(defclass swatch (gtk:widget)
  ((red :initform 0.5 :initarg :red :accessor swatch-red :property :double)
   (snapshots :initform 0 :accessor swatch-snapshots))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispSwatch"))

(gobject:define-vfunc (swatch :measure) (widget orientation for-size)
  (declare (ignore widget for-size))
  (if (eq orientation :horizontal) (values 120 200 -1 -1) (values 40 60 -1 -1)))

(gobject:define-vfunc (swatch :snapshot) (widget snapshot)
  (incf (swatch-snapshots widget))
  (gtk:snapshot-append-color snapshot
                             (gdk:make-rgba :red (swatch-red widget) :green 0.2 :blue 0.2 :alpha 1.0)
                             (graphene:make-rect :origin (graphene:make-point :x 0.0 :y 0.0)
                                                 :size (graphene:make-size :width 10.0 :height 10.0))))

(define-test widget-vfuncs :parent subclass
  (with-gtk
   (without-callback-errors
    (let ((sw (make-instance 'swatch)))
      (is equal '(120 200 -1 -1) (multiple-value-list (gtk:widget-measure sw :horizontal -1)))
      (is equal '(40 60 -1 -1) (multiple-value-list (gtk:widget-measure sw :vertical -1)))
      (let ((window (gtk:window-new)))
        (gtk:window-set-child window sw)
        (gtk:window-present window)
        (iterate-until (lambda () (plusp (swatch-snapshots sw))) :timeout 5)
        (true (plusp (swatch-snapshots sw)))
        (is = 200 (gtk:widget-get-width sw))
        (gtk:window-destroy window))))))

(defclass greeter (gtk:box)
  ((entry :template-child t :reader greeter-entry)
   (button :template-child "greet-button" :reader greeter-button)
   (greeting :initform "Hello" :initarg :greeting :accessor greeting :property :string)
   (greeted :initform nil :accessor greeted)
   (clicks :initform 0 :accessor greeter-clicks))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispGreeter")
  (:template "<interface>
  <template class=\"TestLispGreeter\" parent=\"GtkBox\">
    <property name=\"orientation\">vertical</property>
    <child><object class=\"GtkEntry\" id=\"entry\">
      <signal name=\"activate\" handler=\"greeter-activate\" object=\"TestLispGreeter\" swapped=\"yes\"/>
    </object></child>
    <child><object class=\"GtkButton\" id=\"greet-button\">
      <property name=\"label\">Greet</property>
      <signal name=\"clicked\" handler=\"greeter_clicked\" object=\"TestLispGreeter\"/>
    </object></child>
  </template>
</interface>"))

(defun greeter-activate (greeter entry)
  (setf (greeted greeter) (format nil "~a, ~a!" (greeting greeter) (gtk:editable-get-text entry))))

(defun greeter-clicked (greeter button)
  ;; With object="..." GTK passes that object first (swapped is the default).
  (declare (ignore button))
  (incf (greeter-clicks greeter)))

(define-test composite-templates :parent subclass
  (with-gtk
    (let ((g (make-instance 'greeter :greeting "Hi")))
      (true (typep (greeter-entry g) 'gtk:entry))
      (is string= "Greet" (gtk:button-get-label (greeter-button g)))
      (is eq :vertical (gtk:orientable-get-orientation g))
      (gtk:editable-set-text (greeter-entry g) "Lisp")
      (gobject:emit (greeter-entry g) :activate)
      (is string= "Hi, Lisp!" (greeted g))
      (gobject:emit (greeter-button g) :clicked)
      (is = 1 (greeter-clicks g)))))

(define-test builder-creates-lisp-classes :parent subclass
  (with-gtk
    (let* ((b (gtk4:make-builder
               :string "<interface><object class=\"TestLispGreeter\" id=\"g\">
                          <property name=\"greeting\">Yo</property></object></interface>"))
           (g (gtk:builder-get-object b "g")))
      (is eq 'greeter (type-of g))
      (is string= "Yo" (greeting g))
      (true (typep (greeter-entry g) 'gtk:entry)))))

(define-test builder-handlers-are-lisp-functions :parent subclass
  (with-gtk
    (let* ((b (gtk4:make-builder
               :string "<interface><object class=\"GtkButton\" id=\"b\">
                          <signal name=\"clicked\" handler=\"note-builder-click\"/>
                        </object></interface>"))
           (button (gtk:builder-get-object b "b")))
      (setf *builder-clicks* 0)
      (gobject:emit button :clicked)
      (is = 1 *builder-clicks*))))


;;; Redefinition at the REPL

(defclass redefinable (gobject:object)
  ((a :initform 1 :accessor redefinable-a))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispRedefinable"))

(define-test class-redefinition :parent subclass
  (let* ((old (make-instance 'redefinable))
         (gtype (gobject:class-gtype 'redefinable)))
    ;; New Lisp slots: same GType, and existing instances gain the slot.
    (eval '(defclass redefinable (gobject:object)
            ((a :initform 1 :accessor redefinable-a)
             (b :initform 2 :accessor redefinable-b))
            (:metaclass gobject:gobject-class)
            (:gtype-name "TestLispRedefinable")))
    (let ((new (make-instance 'redefinable)))
      (is = gtype (gobject:class-gtype 'redefinable))
      (is = 2 (funcall 'redefinable-b old))
      (is = 2 (funcall 'redefinable-b new)))
    ;; A new property cannot be added to a registered GType: say so.
    (eval '(defclass redefinable (gobject:object)
            ((a :initform 1 :accessor redefinable-a)
             (b :initform 2 :accessor redefinable-b :property :int))
            (:metaclass gobject:gobject-class)
            (:gtype-name "TestLispRedefinable")))
    (of-type warning (handler-case (progn (make-instance 'redefinable) nil)
                       (warning (w) w)))))

;;; The M3 gate: redefine a running widget from another thread, as an
;;; editor's REPL thread would, while the main loop runs on the main thread.

(defclass live-widget (gtk:widget)
  ((drawn-by :initform nil :accessor drawn-by))
  (:metaclass gobject:gobject-class)
  (:gtype-name "TestLispLiveWidget"))

(gobject:define-vfunc (live-widget :snapshot) (widget snapshot)
  (declare (ignore snapshot))
  (setf (drawn-by widget) :original))

(define-test live-redefinition :parent subclass
  (with-gtk
   (without-callback-errors
    (let ((app (gtk:application-new "org.lisp.gtk4.TestLive" '(:non-unique)))
          (widget nil)
          (seen '()))
      (gobject:connect app :activate
                       (lambda (app)
                         (let ((window (gtk:application-window-new app)))
                           (setf widget (make-instance 'live-widget :width-request 50 :height-request 50))
                           (gtk:widget-add-tick-callback
                            widget (lambda (w clock)
                                     (declare (ignore clock))
                                     (pushnew (drawn-by w) seen)
                                     (gtk:widget-queue-draw w)
                                     t))
                           (gtk:window-set-child window widget)
                           (gtk:window-present window))))
      ;; The "REPL": a second thread that redefines the vfunc, then quits.
      (sb-thread:make-thread
       (lambda ()
         (sleep 1)
         (eval '(gobject:define-vfunc (live-widget :snapshot) (widget snapshot)
                 (declare (ignore snapshot))
                 (setf (drawn-by widget) :redefined)))
         (sleep 1)
         (glib:in-main-thread () (gio:application-quit app))))
      (glib:with-gtk-float-traps (gio:application-run app nil))
      (true (member :original seen))
      (true (member :redefined seen))
      ;; Leave the original definition for other tests.
      (gobject:define-vfunc (live-widget :snapshot) (widget snapshot)
        (declare (ignore snapshot))
        (setf (drawn-by widget) :original))))))
