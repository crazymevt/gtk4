# Threads and macOS

GTK is single-threaded. All GTK calls must happen on the thread running the main loop, and on macOS that must be the process's first thread.

## Running GTK

Start GTK from a script, so it runs on the first thread:

```
sbcl --load my-app.lisp
```

`(gio:application-run app nil)` then runs the main loop until the application quits.

## Working from another thread

`glib:in-main-thread` runs code on the GTK thread. With `:wait t` it blocks and returns the result, re-signalling any error in the calling thread:

```
(glib:in-main-thread (:wait t)
  (gtk:label-set-text label "Updated from a worker thread"))
```

The debugger then shows the calling thread's stack, which ends in the wait. `(glib:gui-thread-backtrace condition)` returns the GTK thread's backtrace from where the error was signalled, as a string. Cadre shows it on the Debugger page.

## Using a REPL with GTK on macOS

Editors like SLIME and Sly run your REPL in a separate thread, which macOS does not allow to create windows. Start the editor's server first, then give the first thread to GTK:

```
;; dev.lisp, run with: sbcl --load dev.lisp
(ql:quickload '(:swank :gtk4))
(swank:create-server :port 4005 :dont-close t)
(load "my-app.lisp")
(my-app:main)            ; runs the GTK main loop on this thread
```

Connect your editor to port 4005. Code you evaluate runs in the REPL thread; wrap GTK calls in `glib:in-main-thread`, or connect handlers by symbol and redefine the functions, which then run on the GTK thread.

## Floating-point traps

SBCL normally signals errors for floating-point conditions that GTK, cairo and graphics drivers trigger routinely. The bindings disable these traps around every call into GTK and every callback. If you run the main loop yourself, wrap it in `glib:with-gtk-float-traps`.
