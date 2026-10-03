;;;; cairo.lisp — the hand-written cairo spec, end to end

(in-package #:gtk4-tests)

(define-test cairo :parent gtk4-tests)

(defun pixel (surface x y)
  "The ARGB32 pixel at X, Y of an image SURFACE."
  (cairo:surface-flush surface)
  (cffi:mem-ref (cairo:image-surface-get-data surface) :uint32
                (+ (* y (cairo:image-surface-get-stride surface)) (* x 4))))

(define-test draw-on-image-surface :parent cairo
  (let* ((surface (cairo:image-surface-create :argb32 20 10))
         (cr (cairo:create surface)))
    (is eq :success (cairo:status cr))
    (cairo:set-source-rgb cr 1 0 0)
    (cairo:paint cr)
    (is = #xffff0000 (pixel surface 0 0))
    ;; A blue rectangle on the right half.
    (cairo:set-source-rgb cr 0 0 1)
    (cairo:rectangle cr 10 0 10 10)
    (cairo:fill cr)
    (is = #xffff0000 (pixel surface 5 5))
    (is = #xff0000ff (pixel surface 15 5))
    (is = 20 (cairo:image-surface-get-width surface))
    (is eq :argb32 (cairo:image-surface-get-format surface))))

(define-test paths-and-out-values :parent cairo
  (let ((cr (cairo:create (cairo:image-surface-create :argb32 100 100))))
    (cairo:move-to cr 10 20)
    (cairo:line-to cr 50 60)
    (is equal '(50d0 60d0) (multiple-value-list (cairo:get-current-point cr)))
    (true (cairo:has-current-point cr))
    (is equal '(10d0 20d0 50d0 60d0) (multiple-value-list (cairo:path-extents cr)))
    (cairo:set-line-width cr 4)
    (is = 4d0 (cairo:get-line-width cr))
    (cairo:set-line-cap cr :round)
    (is eq :round (cairo:get-line-cap cr))
    (cairo:set-dash cr '(4 2) 0)
    (is = 2 (cairo:get-dash-count cr))))

(define-test matrices :parent cairo
  (let ((m (cairo:matrix-init-translate 5 7)))
    (true (typep m 'cairo:matrix))
    (is = 5d0 (cairo:matrix-x0 m))
    (is = 7d0 (cairo:matrix-y0 m))
    (cairo:matrix-scale m 2 2)
    (is = 2d0 (cairo:matrix-xx m))
    (let ((cr (cairo:create (cairo:image-surface-create :argb32 10 10))))
      (cairo:set-matrix cr m)
      (is = 5d0 (cairo:matrix-x0 (cairo:get-matrix cr))))
    (is eq :success (cairo:matrix-invert m))
    (is = 0.5d0 (cairo:matrix-xx m))))

(define-test text-extents :parent cairo
  (let ((cr (cairo:create (cairo:image-surface-create :argb32 200 50))))
    (cairo:select-font-face cr "Sans" :normal :bold)
    (cairo:set-font-size cr 20)
    (let ((extents (cairo:text-extents cr "Hello")))
      (true (typep extents 'cairo:text-extents))
      (true (plusp (cairo:text-extents-width extents)))
      (true (plusp (cairo:text-extents-x-advance extents))))
    (true (plusp (cairo:font-extents-height (cairo:font-extents cr))))))

(define-test gradients :parent cairo
  (let ((gradient (cairo:pattern-create-linear 0 0 100 0)))
    (cairo:pattern-add-color-stop-rgb gradient 0 1 0 0)
    (cairo:pattern-add-color-stop-rgba gradient 1 0 0 1 0.5)
    (is equal '(:success 2) (multiple-value-list (cairo:pattern-get-color-stop-count gradient)))
    (is eq :linear (cairo:pattern-get-type gradient))
    (let* ((surface (cairo:image-surface-create :argb32 100 10))
           (cr (cairo:create surface)))
      (cairo:set-source cr gradient)
      (cairo:paint cr)
      ;; Mostly red at the left edge, mostly blue at the right.
      (let ((left (pixel surface 0 0))
            (right (pixel surface 99 0)))
        (true (> (ldb (byte 8 16) left) 240))
        (true (> (ldb (byte 8 0) right) 100))
        (true (< (ldb (byte 8 16) right) 10))))))

(define-test png-round-trip :parent cairo
  (let* ((path (format nil "/tmp/gtk4-cairo-test-~d.png" (random 1000000)))
         (surface (cairo:image-surface-create :rgb24 8 8))
         (cr (cairo:create surface)))
    (cairo:set-source-rgb cr 0 1 0)
    (cairo:paint cr)
    (unwind-protect
         (progn
           (is eq :success (cairo:surface-write-to-png surface path))
           (let ((loaded (cairo:image-surface-create-from-png path)))
             (is eq :success (cairo:surface-status loaded))
             (is = 8 (cairo:image-surface-get-width loaded))
             (is = #xff00ff00 (pixel loaded 3 3))))
      (when (probe-file path) (delete-file path)))))

(define-test regions :parent cairo
  (let ((region (cairo:region-create)))
    (true (cairo:region-is-empty region))
    (cairo:region-union-rectangle region (cairo:make-rectangle-int :x 0 :y 0 :width 10 :height 10))
    (true (cairo:region-contains-point region 5 5))
    (false (cairo:region-contains-point region 15 5))
    (is = 10 (cairo:rectangle-int-width (cairo:region-get-extents region)))))

(define-test cairo-in-gsk :parent cairo
  ;; GSK cairo nodes hand out a cairo context, the way GTK widgets draw.
  (with-gtk
    (let* ((bounds (graphene:rect-init (graphene:rect-alloc) 0 0 30 30))
           (node (gsk:cairo-node-new bounds))
           (cr (gsk:cairo-node-get-draw-context node)))
      (cairo:set-source-rgb cr 0 0 1)
      (cairo:arc cr 15 15 10 0 (* 2 pi))
      (cairo:fill cr)
      (is eq :success (cairo:status cr))
      (true (cairo:image-surface-get-width (gsk:cairo-node-get-surface node))))))

(define-test version :parent cairo
  (true (stringp (cairo:version-string))))
