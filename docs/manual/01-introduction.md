# Introduction

gtk4 gives SBCL programs the whole GTK 4 stack: GLib, GObject, GIO, Pango, cairo, Graphene, GDK, GSK and GTK. The bindings are generated ahead of time from GObject Introspection data, so every function GTK documents is available, with the same name rules everywhere and a docstring that links to the upstream page.

If you know GTK, you already know this library. This manual covers only what differs from C: how names are formed, how values are converted, how memory and threads work, and how to write callbacks.

## Installing

You need SBCL with Quicklisp and GTK 4.14 or newer.

On macOS:

```
brew install sbcl gtk4
```

On Debian or Ubuntu, install `sbcl` and `libgtk-4-1`. Then put the project where ASDF can find it (for example under `~/quicklisp/local-projects/`) and load it:

```
(ql:quickload :gtk4)
```

## A first program

```
(defun activate (app)
  (let ((window (gtk:application-window-new app))
        (button (gtk:button-new-with-label "Say hello")))
    (gtk:window-set-title window "Hello")
    (gobject:connect button :clicked
                     (lambda (button)
                       (declare (ignore button))
                       (print "Hello from Lisp!")))
    (gtk:window-set-child window button)
    (gtk:window-present window)))

(let ((app (gtk:application-new "org.example.Hello" '(:default-flags))))
  (gobject:connect app :activate #'activate)
  (gio:application-run app nil))
```

Run it from a script (`sbcl --load hello.lisp`): on macOS, GTK must run on the process's first thread. The chapter on threads explains how to combine this with a REPL.

## Packages

Each GIR namespace has its own package: `glib`, `gobject`, `gio`, `cairo`, `pango`, `pango-cairo`, `graphene`, `gdk-pixbuf`, `gdk`, `gsk`, `gtk`, `harfbuzz`. The `gtk4` package holds helpers that span them. The packages are meant to be used with their prefixes; many names (such as `gtk:window` or `cairo:fill`) would clash with each other, or with Common Lisp, if you imported them.
