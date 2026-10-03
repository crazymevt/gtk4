# gtk4 — GTK 4 for SBCL

Complete GTK 4 bindings for SBCL, generated ahead of time from GObject
Introspection data, with an idiomatic CLOS layer on top.

**Status:** M0 through M2 are complete; M3 (subclassing and the Lisp layer) is in
review. The whole stack, GLib through GTK plus cairo, is generated, committed and
tested: 98% of bindable functions, and 750 virtual functions Lisp classes can override.

## A taste

```lisp
;; A widget class defined in Lisp: its own GType, a property, a virtual function.
(defclass swatch (gtk:widget)
  ((color :initform "rebeccapurple" :accessor swatch-color :property :string))
  (:metaclass gobject:gobject-class)
  (:gtype-name "MySwatch"))

(gobject:define-vfunc (swatch :snapshot) (widget snapshot)
  (gtk:snapshot-append-color snapshot (color-of widget) (bounds-of widget)))

;; A window from one s-expression.
(gtk:build
  (gtk:window :title "Hello"
    (gtk:box :orientation :vertical :spacing 6
      (make-instance 'swatch :vexpand t)
      (gtk:button :label "Greet" :on-clicked 'greet))))
```

Every C function is also available under its own name (`gtk:widget-set-visible`
for `gtk_widget_set_visible`), with a docstring linking to its upstream page.

Design document: <https://claude.ai/code/artifact/893d30d2-ba38-4d3e-a255-35532f0ea5da>

## Requirements

- SBCL 2.4 or newer, with Quicklisp
- GTK 4.14 or newer
- For the generator only: the `.gir` files for GTK and its dependencies
  (`cairo-1.0.gir` and `freetype2-2.0.gir` come from the `gobject-introspection` package)

macOS:

```sh
brew install sbcl gtk4 gobject-introspection
```

## Layout

| Path | Contents |
| --- | --- |
| `src/runtime/` | Hand-written runtime: library loading, objects and memory, signals, callbacks, marshalling, Lisp-defined GTypes |
| `src/gtk4/` | The Lisp layer: templates, list models, `gtk:build`, CSS, `gio:async` |
| `src/generated/` | Generated bindings, one file per namespace, plus `COVERAGE.md` |
| `generator/` | GIR parser, marshalling planner and code emitter; maintainers only |
| `scripts/` | Maintenance scripts |
| `examples/` | Runnable examples |
| `docs/manual/` | The manual, rendered with the reference site by `make docs` |
| `demos/` | The demo collection and its browser (`gtk4-demo` system) |
| `tests/` | Parachute test suite |

## Common tasks

```sh
make test               # run the test suite
make stress             # run the leak/stress suite (the M1 gate)
make generate           # regenerate src/generated/ (run after changing the generator)
make docs               # build the reference site and manual into build/docs/
make full-stack         # time a clean compile and a cached load of everything
make summary            # parse every target .gir file and print what it contains
make hello              # open the hello-world window
make hello QUIT_AFTER=3 # same, quitting after 3 seconds
make example NAME=clock     # run examples/clock.lisp (a custom widget and a template)
make example NAME=todo      # run examples/todo.lisp (the Lisp layer)
make demo               # the demo browser: 25 demos to read and run
```

## License

MIT. See [LICENSE](LICENSE).
