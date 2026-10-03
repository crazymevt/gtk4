# gtk4 — GTK 4 for SBCL

Complete GTK 4 bindings for SBCL, generated ahead of time from GObject
Introspection data, with an idiomatic CLOS layer on top.

**Status:** M0 and M1 are complete; M2 is in progress. The whole stack, GLib through
GTK plus cairo, is generated, committed and tested: 98% of bindable functions.

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
| `src/runtime/` | Hand-written runtime: library loading, objects and memory, signals, callbacks, marshalling |
| `src/generated/` | Generated bindings, one file per namespace, plus `COVERAGE.md` |
| `generator/` | GIR parser, marshalling planner and code emitter; maintainers only |
| `scripts/` | Maintenance scripts |
| `examples/` | Runnable examples |
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
make example NAME=drawing   # run examples/drawing.lisp (cairo in a GtkDrawingArea)
make demo               # the demo browser: 21 demos to read and run
```

## License

MIT. See [LICENSE](LICENSE).
