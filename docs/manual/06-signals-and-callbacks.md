# Signals and callbacks

## Signals

`gobject:connect` calls a function whenever an object emits a signal. The function receives the object, then the signal's arguments; its return value becomes the signal's return value.

```
(gobject:connect button :clicked
                 (lambda (button) (gtk:button-set-label button "Clicked")))

(gobject:connect entry "notify::text"     ; a detailed signal, as a string
                 (lambda (entry pspec) (declare (ignore pspec))
                   (print (gtk:editable-get-text entry))))
```

`connect` returns a handler id for `gobject:disconnect`, `gobject:block-handler` and `gobject:unblock-handler`. `gobject:emit` emits a signal from Lisp:

```
(gobject:emit button :clicked)
```

Each type's reference page lists its signals with the handler's arguments.

## Callbacks

Where GTK takes a callback (a draw function, a sort function, an async completion), pass a Lisp function. The `user_data` and destroy-notify parameters disappear:

```
(gio:file-read-async file glib:+priority-default+ nil
                     (lambda (source result)
                       (let ((stream (gio:file-read-finish source result)))
                         ...)))

(glib:timeout-add-seconds glib:+priority-default+ 1
                          (lambda () (print "tick") t))   ; return nil to stop
```

The bindings keep each callback alive exactly as long as GTK needs it, following GTK's annotations: until the function returns, until the one completion call, or until GTK says it is done.

## Changing a running program

Pass a symbol instead of a function, and it is looked up on every call. Redefining the function at the REPL changes the running program immediately:

```
(defun on-click (button) (gtk:button-set-label button "First"))
(gobject:connect button :clicked 'on-click)
;; later, at the REPL:
(defun on-click (button) (gtk:button-set-label button "Second"))
```

## Errors in callbacks

An error inside a handler never unwinds into GTK's C code. It is passed to `gtk4.runtime:*callback-error-handler*`, which by default prints it and carries on. During development you can bind it to a function that calls `invoke-debugger`, to inspect the error in place.
