# The Lisp layer

Every C function is available under its own name, so GTK's documentation applies directly. On top of those bindings, a few helpers make common tasks shorter in Lisp. They never hide the C-shaped functions; mix the two freely.

## List models of Lisp values

GTK's list widgets display a `GListModel` of GObjects. `gobject:lisp-object` is a GObject that carries any Lisp value, so a model can hold strings, numbers, structures or CLOS instances.

```
(gio:make-list-store :items '("one" 2 :three))     ; wraps each value
(gio:list-model-items store)                         ; => ("one" 2 :three)
(gobject:lisp-object-value (gio:list-model-get-item store 0))   ; => "one"
```

`gtk:make-list-view` builds a list view from a model or a plain sequence. It takes two functions: `:setup` creates the widget for a row, and `:bind` shows an item in it:

```
(gtk:make-list-view (list apple pear plum)
  :setup (lambda () (make-instance 'gtk:label :xalign 0.0))
  :bind (lambda (label fruit) (gtk:label-set-text label (fruit-name fruit))))
```

`:selection` is `:single` (the default), `:multiple` or `:none`. `gtk:make-factory` builds the same kind of list item factory for grid views, column views and drop-downs, and `gtk:list-item-value` gives a list item's unwrapped value.

## Building widget trees

`gtk:build` creates a widget tree from an s-expression. It returns the root widget and a hash table of the objects given an `:id`:

```
(multiple-value-bind (window ids)
    (gtk:build
      (gtk:window :title "Greeter"
        (gtk:header-bar :child-type "titlebar")
        (gtk:grid :column-spacing 6 :row-spacing 6
          (gtk:label :label "Name" :layout (:column 0 :row 0))
          (gtk:entry :id :name :layout (:column 1 :row 0))
          (gtk:button :label "Greet" :on-clicked 'greet :layout (:column 1 :row 1)))))
  (gtk:window-present window)
  (gethash :name ids))                    ; the entry
```

Each node is a class name followed by initargs and children. Initargs are properties or Lisp slots, and their values are evaluated. A few keys are special:

| Key | Meaning |
| --- | --- |
| `:id` | Record the object in the returned table |
| `:on-clicked`, `:on-notify`, ... | Connect the signal to a function (or a symbol naming one) |
| `:child-type` | How the parent adds this child, as `<child type="...">` in a .ui file: `"titlebar"`, `"start"`, `"end"`, `"overlay"` |
| `:layout` | Properties of the child's layout child, as `<layout>` in a .ui file: `(:column 1 :row 0)` in a grid |

Children are added the way GtkBuilder adds them, so every container behaves as it does in a `.ui` file. A child may also be any form that returns a widget.

## CSS

`gtk:css` writes CSS from s-expressions, and `gtk:add-css` loads CSS for the whole display. `gtk:add-css` takes CSS text or rules:

```
(gtk:add-css '((".title" :font-size "20pt" :font-weight :bold)
               ("button.suggested" :background "#3584e4" :color :white)
               ("box.toolbar" :padding ("4px" "8px"))))
```

## Asynchronous calls

GIO splits an asynchronous operation into a function that starts it, such as `file_load_contents_async`, and one that finishes it from a callback, such as `file_load_contents_finish`. `gio:async` joins them. Omit the callback argument. The first function receives the finish function's values; on failure, the `:error` function receives the condition:

```
(gio:async (gio:file-load-contents-async file)
           (lambda (ok contents etag)
             (declare (ignore ok etag))
             (show-text contents))
           :error (lambda (e) (show-error (glib:glib-error-message e))))
```

Optional arguments before the callback, usually the `GCancellable`, may be left out. It works for every async function whose finish function GIR names or that follows the naming convention: file and network I/O, `gtk:file-dialog-open`, `gtk:alert-dialog-choose`, clipboard reads, and many more.

## Errors

Every GError domain has a condition class, and each of its codes has a subclass, so `handler-case` can be precise:

```
(handler-case (gio:file-load-contents file nil)
  (gio:io-error-not-found () (create-default file))
  (gio:io-error (e) (warn "Could not read ~a: ~a" file (glib:glib-error-message e))))
```

All of them are subclasses of `glib:glib-error`. `glib:glib-error-keyword` gives the code as a keyword of the domain's enum, such as `:not-found`.
