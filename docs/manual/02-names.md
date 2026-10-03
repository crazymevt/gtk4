# Names and documentation

Every Lisp name is derived from the C name by fixed rules, so you can read GTK's documentation and write the Lisp directly.

| C | Lisp |
| --- | --- |
| `GtkWidget` (type) | `gtk:widget` |
| `gtk_widget_set_visible` (method) | `gtk:widget-set-visible` |
| `gtk_button_new_with_label` (constructor) | `gtk:button-new-with-label` |
| `gtk_init` (function) | `gtk:init` |
| `GtkWidget:visible` (property) | `gtk:widget-visible`, settable with `setf` |
| `GtkButton::clicked` (signal) | `:clicked` with `gobject:connect` |
| `GTK_ALIGN_CENTER` (enum value) | `:center` |
| `GTK_STYLE_PROVIDER_PRIORITY_USER` (constant) | `gtk:+style-provider-priority-user+` |
| `GdkRGBA.red` (struct field) | `gdk:rgba-red`, settable with `setf` |
| `cairo_move_to` | `cairo:move-to` |

The rules: drop the library prefix, split CamelCase into words, use hyphens instead of underscores, and prefix methods with their type. Constants are wrapped in `+`. A few cases differ from the pattern:

- When GTK marks one function as a replacement for another, the replacement takes the plain name: `gtk_list_store_newv` is `gtk:list-store-new`.
- Variadic C functions are not bound; GTK always provides an array-taking version.
- GLib's own container API (`g_list_append`, `g_hash_table_insert`, …) is left out on purpose: lists and hash tables are ordinary Lisp values here.

## Finding things

Every generated function, class and enum has a docstring: a one-paragraph summary rewritten with Lisp names, what it returns, the C name and a link to the upstream page.

```
(documentation 'gtk:button-new-with-label 'function)
```

The `gtk4` package moves between the two worlds:

```
(gtk4:lisp-name "gtk_widget_set_visible")   ; => GTK:WIDGET-SET-VISIBLE
(gtk4:c-name 'gtk:widget-set-visible)       ; => "gtk_widget_set_visible"
(gtk4:documentation-url 'gtk:button)        ; => "https://docs.gtk.org/gtk4/class.Button.html"
(gtk4:browse 'gtk:button)                   ; opens that page
```

The reference section of this site has one page per type, laid out like docs.gtk.org, with the Lisp form of every function.
