# Custom widgets

A CLOS class can define a new GType: a widget, a list model, any GObject. GTK then treats it like one of its own classes. It can appear in a `.ui` file, be found by `g_type_from_name`, have properties that bindings and expressions use, and override the virtual functions GTK calls.

## Defining a class

Give the class the `gobject:gobject-class` metaclass and a GType name of its own:

```
(defclass clock-face (gtk:widget)
  ((show-seconds :initform t :accessor show-seconds :property :boolean)
   (last-drawn :initform nil :accessor last-drawn))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispClockFace"))
```

The GType is registered the first time it is needed, usually by the first `make-instance`. It derives from the nearest GObject class among the superclasses, here GtkWidget. Instances made by C code, such as a GtkBuilder file naming `LispClockFace`, get a Lisp proxy with their slots initialized like any other.

A class without `:gtype-name` shares its parent's GType. It can add Lisp slots and methods, but not properties, signals or virtual functions.

## Virtual functions

GTK calls virtual functions to ask a widget to draw (`snapshot`), to say how big it wants to be (`measure`), to lay out children (`size_allocate`), and so on. `gobject:define-vfunc` implements one in Lisp:

```
(gobject:define-vfunc (clock-face :measure) (widget orientation for-size)
  (declare (ignore widget orientation for-size))
  (values 120 240 -1 -1))       ; minimum, natural, and two baselines

(gobject:define-vfunc (clock-face :snapshot) (widget snapshot)
  (let ((cr (gtk:snapshot-append-cairo snapshot (bounds widget))))
    (draw-clock widget cr)))
```

The name is the C name in kebab case, as a keyword: GTK's `snapshot` is `:snapshot`, and `size_allocate` is `:size-allocate`. Each virtual function's docstring and upstream link are in the reference, under its class (for example, `vfunc.Widget.measure.html`). The arguments follow the C order, starting with the instance. Out arguments, like `measure`'s `minimum` and `natural`, are returned as values instead.

Inside the body, `(call-next-vfunc)` calls the next implementation: a Lisp superclass's, or else the C parent class's. With arguments, it passes those instead, starting with the instance:

```
(gobject:define-vfunc (my-entry :activate) (entry)
  (log-activation entry)
  (call-next-vfunc))
```

When a name is ambiguous, give the class or interface that declares it: `(gobject:define-vfunc (my-model :get-item gio:list-model) ...)`.

`gobject:call-vfunc` calls an object's implementation directly, as GTK would: `(gobject:call-vfunc widget :measure :horizontal -1)`.

Virtual functions that report errors (they take a `GError **` in C) signal a condition to report one. A `glib:glib-error` keeps its domain and code; any other error is passed on with the domain `gtk4-lisp-error-quark`.

## Interfaces

List an interface class among the superclasses and implement its virtual functions. A list model of Lisp values:

```
(defclass number-list (gobject:object gio:list-model)
  ((items :initarg :items :accessor items))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispNumberList"))

(gobject:define-vfunc (number-list :get-item-type) (model)
  (declare (ignore model))
  (gobject:class-gtype 'gobject:lisp-object))

(gobject:define-vfunc (number-list :get-n-items) (model)
  (length (items model)))

(gobject:define-vfunc (number-list :get-item) (model position)
  (gobject:make-lisp-object (nth position (items model))))
```

To override an interface's functions that a C parent class already implements, list the interface again among the direct superclasses, as `gtk4:lisp-builder-scope` does with `gtk:builder-scope`.

## Properties

A slot with the `:property` option is also a GObject property:

```
(seconds :initform 0 :accessor clock-seconds
         :property (:int :min 0 :max 59 :nick "Seconds"))
```

The value lives in the slot. Writing it from Lisp, through the accessor or `setf slot-value`, follows the same rules as `g_object_set_property`: a numeric property takes a number of its type within `:min` and `:max`, and anything else signals `gobject:property-value-error`. A valid write emits `notify::seconds` as C code expects. C code reads and writes it through `g_object_get` and `g_object_set`, GtkBuilder sets it from XML, and `g_object_bind_property` binds it.

The option is a type, or a list of a type and keyword options. The types are `:boolean`, `:int`, `:uint`, `:long`, `:ulong`, `:int64`, `:uint64`, `:float`, `:double`, `:string`, `:pointer` and `:gtype`, plus `(:enum gtk:orientation)`, `(:flags gtk:state-flags)`, `(:object gtk:widget)` and `(:boxed "GdkRGBA")`. The options are `:nick`, `:blurb` (defaulting to the slot's documentation), `:min`, `:max`, `:default` (defaulting to a constant initform), and `:flags`. `:flags` is a list of `:readable`, `:writable`, `:construct`, `:construct-only` and `:explicit-notify`, and defaults to readable and writable.

## Signals

The `:signals` class option adds signals. Each is a name, argument types, and options:

```
(:signals (:ticked (:int))
          (:query (:string) :return :boolean :flags (:run-last)))
```

Connect and emit them like GTK's own: `(gobject:connect clock :ticked handler)`, `(gobject:emit clock :ticked 30)`.

## Composite templates

A widget can take its children from a GtkBuilder template. Mark the slots that hold template children with `:template-child`. Its value is the child's id, or `t` for an id equal to the slot's name:

```
(defclass greeter (gtk:box)
  ((entry :template-child t :reader greeter-entry)
   (button :template-child "greet-button" :reader greeter-button))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispGreeter")
  (:template "<interface>
    <template class=\"LispGreeter\" parent=\"GtkBox\">
      <child><object class=\"GtkEntry\" id=\"entry\">
        <signal name=\"activate\" handler=\"greet\" object=\"LispGreeter\"/>
      </object></child>
      <child><object class=\"GtkButton\" id=\"greet-button\">
        <property name=\"label\">Greet</property>
      </object></child>
    </template>
  </interface>"))

(defun greet (greeter entry)
  (format t "Hello, ~a!~%" (gtk:editable-get-text entry)))
```

The template can also come from a file or a GResource. Write `(:template :file "ui/greeter.ui" :system "my-app")` for a path relative to an ASDF system, or `(:template :resource "/org/example/greeter.ui")`.

Signal handlers in the template are Lisp functions, looked up in the package of the class's name. The names `greet`, `"on_greet"` (underscores for hyphens) and `my-app:greet` all work. With `object="..."`, the handler receives that object first, then the signal's arguments, then the emitter last; that's GTK's default when `object` is set. Add `swapped="no"` for the emitter first and the object last.

The same scope is available for any GtkBuilder file: `(gtk4:make-builder :file "window.ui" :package :my-app)`.

## Redefining at the REPL

Lisp classes stay live:

- **Virtual functions:** redefining one with `gobject:define-vfunc` takes effect at once, for existing instances too. Queue a redraw (`gtk:widget-queue-draw`) to see a new `:snapshot`. `gobject:remove-vfunc` goes back to the parent's implementation.
- **Lisp slots and methods:** redefining the class updates them, and existing instances gain new slots, as for any CLOS class.
- **Properties, signals and templates:** GLib fixes these when the GType's class is first initialized. Changing them in a redefinition signals a warning; give the class a new `:gtype-name`, or restart, to apply them.

From an editor, start the program from a script that starts a Swank or Slynk server first; see "Threads and macOS". Then evaluate redefinitions in the editor as usual. `examples/clock.lisp` is set up for this.

## Lifetimes

Lisp instances follow the usual rules (see "Objects and memory"). Lisp state lives as long as either side uses the object. Avoid closures that capture a widget in callbacks the widget itself owns, such as a tick callback that refers to its own widget, because that cycle passes through C, where the garbage collector cannot see it. Use the callback's arguments instead: a tick callback receives its widget.
