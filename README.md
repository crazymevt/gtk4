# gtk4 — GTK 4 for SBCL

Complete GTK 4 bindings for SBCL, generated ahead of time from GObject
Introspection data, with an idiomatic CLOS layer on top.

**Status:** milestone M0 (foundations). Nothing here is usable as a library yet.

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
| `src/runtime/` | Hand-written runtime: library loading, float traps, main-thread checks |
| `generator/` | GIR parser and (soon) code emitter; maintainers only |
| `examples/` | Runnable examples |
| `tests/` | Parachute test suite |

## Common tasks

```sh
make test               # run the test suite
make summary            # parse every target .gir file and print what it contains
make hello              # open the hello-world window
make hello QUIT_AFTER=3 # same, quitting after 3 seconds
```

## License

MIT. See [LICENSE](LICENSE).
