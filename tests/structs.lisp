;;;; structs.lisp — struct layouts: constructors, fields, out-structs, struct arrays

(in-package #:gtk4-tests)

(define-test structs :parent gtk4-tests)

(define-test boxed-constructor-and-fields :parent structs
  (let ((color (gdk:make-rgba :red 1 :green 0.5 :alpha 1)))
    (true (typep color 'gdk:rgba))
    (is = 1.0 (gdk:rgba-red color))
    (is = 0.5 (gdk:rgba-green color))
    (is = 0.0 (gdk:rgba-blue color))
    (setf (gdk:rgba-blue color) 0.25)
    (is = 0.25 (gdk:rgba-blue color))))

(define-test methods-on-constructed-structs :parent structs
  ;; gdk_rgba_parse writes into an existing GdkRGBA.
  (let ((color (gdk:make-rgba)))
    (true (gdk:rgba-parse color "#ff0000"))
    (is = 1.0 (gdk:rgba-red color))
    (is = 0.0 (gdk:rgba-green color))
    (is string= "rgb(255,0,0)" (gdk:rgba-to-string color))))

(define-test plain-record-out-structs :parent structs
  ;; pango_layout_get_pixel_extents fills two caller-allocated PangoRectangles.
  (let* ((font-map (pango-cairo:font-map-get-default))
         (context (pango:font-map-create-context font-map))
         (layout (pango:layout-new context)))
    (pango:layout-set-text layout "Hello" -1)
    (multiple-value-bind (ink logical) (pango:layout-get-pixel-extents layout)
      (true (typep ink 'pango:rectangle))
      (true (plusp (pango:rectangle-width logical)))
      (true (plusp (pango:rectangle-height logical))))))

(define-test boxed-out-structs :parent structs
  (let ((rect (graphene:rect-alloc)))
    (graphene:rect-init rect 10 20 100 50)
    (let ((center (graphene:rect-get-center rect)))
      (true (typep center 'graphene:point))
      (is = 60.0 (graphene:point-x center))
      (is = 45.0 (graphene:point-y center)))))

(define-test text-iters :parent structs
  (with-gtk
    (let ((buffer (gtk:text-buffer-new nil)))
      (gtk:text-buffer-set-text buffer "hello world" -1)
      (let ((start (gtk:text-buffer-get-start-iter buffer))
            (end (gtk:text-buffer-get-end-iter buffer)))
        (is = 0 (gtk:text-iter-get-offset start))
        (is = 11 (gtk:text-iter-get-offset end))
        (is string= "hello world" (gtk:text-buffer-get-text buffer start end t))
        ;; Iterators are independent copies.
        (gtk:text-iter-forward-word-end start)
        (is = 5 (gtk:text-iter-get-offset start))
        (is = 11 (gtk:text-iter-get-offset end))))))

(define-test struct-arrays :parent structs
  (with-gtk
    ;; gsk_linear_gradient_node_new takes an inline array of GskColorStop.
    (let* ((bounds (graphene:rect-init (graphene:rect-alloc) 0 0 100 10))
           (start (graphene:point-init (graphene:point-alloc) 0 0))
           (end (graphene:point-init (graphene:point-alloc) 100 0))
           (stops (list (gsk:make-color-stop :offset 0 :color (gdk:make-rgba :red 1 :alpha 1))
                        (gsk:make-color-stop :offset 1 :color (gdk:make-rgba :blue 1 :alpha 1))))
           (node (gsk:linear-gradient-node-new bounds start end stops)))
      (true node)
      (is = 2 (gsk:linear-gradient-node-get-n-color-stops node))
      (is = 1.0 (gdk:rgba-blue (gsk:color-stop-color (second stops)))))))

(define-test gvalue-out-arguments :parent structs
  (with-gtk
    ;; gtk_tree_model_get_value fills a caller-allocated GValue.
    (let* ((store (gtk:list-store-new (list (rt:gtype-from-name "gchararray"))))
           (iter (gtk:list-store-append store)))
      (rt:with-gvalue (v (rt:gtype-from-name "gchararray"))
        (rt:set-gvalue v "cell")
        (gtk:list-store-set-value store iter 0 v))
      (is string= "cell" (gtk:tree-model-get-value store iter 0)))))
