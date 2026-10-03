# Objects and memory

Every GObject reaching Lisp is represented by exactly one proxy object, an instance of a CLOS class that mirrors the GType hierarchy: `gtk:button` is a subclass of `gtk:widget`, and also of the interface class `gtk:actionable`. Because there is only one proxy per object, `eq` works.

## Creating objects

Use the C constructors, or `make-instance` with properties as initargs:

```
(gtk:button-new-with-label "Save")
(make-instance 'gtk:button :label "Save" :halign :center)
```

## Properties

Each property has an accessor, settable with `setf`. `gobject:property` reads or writes any property by name:

```
(gtk:widget-visible button)                 ; read
(setf (gtk:button-label button) "Saved")    ; write
(gobject:property button :label)            ; by name
```

## Memory

You never free objects yourself. The rules:

- Lisp holds one reference to each object it has seen.
- While C code also uses the object (a widget inside a window, an item in a list store), its proxy stays alive, together with any Lisp state and signal handlers, even if Lisp keeps no reference.
- Once only Lisp holds it and Lisp drops the proxy, the garbage collector frees both.
- References are released on the GTK thread, never from the garbage collector's thread.

Top-level windows are the exception, as in C: GTK owns them, and you close them with `gtk:window-destroy` (or by the user closing them).

A signal handler that refers to its own object, such as a button handler that changes the button, does not keep it alive forever: handlers belong to their object's proxy and are freed with it.
