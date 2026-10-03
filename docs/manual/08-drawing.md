# Drawing with cairo

`gtk:drawing-area` is the simplest way to draw your own content. Give it a draw function; GTK calls it with a cairo context and the area's size whenever it needs repainting.

```
(defun draw (area cr width height)
  (declare (ignore area))
  (cairo:set-source-rgb cr 0.2 0.4 0.8)
  (cairo:paint cr)
  (cairo:arc cr (/ width 2) (/ height 2) (* 0.4 (min width height)) 0 (* 2 pi))
  (cairo:set-source-rgb cr 1 0.8 0.2)
  (cairo:fill cr))

(gtk:drawing-area-set-draw-func area 'draw)
```

Passing `'draw` rather than `#'draw` lets you redefine `draw` at the REPL and see the change on the next repaint (call `gtk:widget-queue-draw` to repaint at once).

## The cairo package

cairo's functions follow the C names without the `cairo_` prefix: `cairo_move_to` is `cairo:move-to`, `cairo_pattern_create_linear` is `cairo:pattern-create-linear`. Each docstring links to the cairo manual. Results that C returns through pointers come back as values:

```
(cairo:get-current-point cr)          ; => x, y
(cairo:text-extents cr "Hello")       ; => a cairo:text-extents
(cairo:matrix-init-rotate (/ pi 4))   ; => a cairo:matrix
```

Surfaces, patterns and contexts are freed automatically. Image surfaces give direct pixel access through `cairo:image-surface-get-data`; call `cairo:surface-flush` before reading and `cairo:surface-mark-dirty` after writing.

See `examples/drawing.lisp` for a complete program.
