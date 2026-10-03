# Changelog

## Unreleased

**Fixes**
- A callback whose Lisp function returns a value C cannot take, such as a keyword from a sort function where C expects a `gint`, no longer crashes the process. The error goes to `*callback-error-handler*` like any other error in a callback, and C receives 0. Integer return values are checked against the C type's range.

## 1.0.0 (2026-10-03)

The first release.

**Bindings**
- The whole GTK 4 stack, generated from GObject Introspection: GLib, GObject, GModule, GIO, cairo, HarfBuzz, Pango, PangoCairo, Graphene, GdkPixbuf, GDK, GSK and GTK. That is 98% of bindable functions (GTK alone 99.5%), with every gap listed with its reason in `src/generated/COVERAGE.md`.
- libadwaita as the optional `gtk4-adwaita` system (99.9% of bindable functions).
- One CLOS proxy per GObject, memory managed through toggle references. Signals take Lisp functions, or symbols for live redefinition. Callbacks follow their GIR scope; GErrors become conditions, with a class per domain and per code.
- Docstrings for every function, with the C name and a link to the upstream page, and a reference site (`make docs`) shaped like docs.gtk.org.

**Custom widgets**
- GObject classes defined in Lisp, overriding 762 virtual functions with `gobject:define-vfunc` and `call-next-vfunc`; redefinition takes effect immediately.
- Lisp slots as GObject properties, Lisp-defined signals, interfaces, and composite templates with Lisp signal handlers.

**The Lisp layer**
- List models of any Lisp value, `gtk:make-list-view`, `gtk:build` for widget trees, `gtk:css`, and `gio:async` for asynchronous calls.

**Deployment**
- `gtk4:save-executable`; macOS application bundles that carry their own GTK (`make app`); a Flatpak manifest.

**Documentation**
- A manual with a tutorial, 25 demos in a browser (`make demo`), and 6 example programs.

**Platforms**
- Linux and macOS (tier 1) and Windows (tier 2), tested in CI with GTK 4.14 and newer and SBCL 2.2.9 and newer.
