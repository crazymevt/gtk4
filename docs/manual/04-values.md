# Values

Arguments and results are converted between Lisp and C automatically, following the GIR annotations.

| C | Lisp |
| --- | --- |
| `gboolean` | `t` or `nil` |
| integers, floats | numbers (floats accept any real) |
| `const char *` | strings (UTF-8) |
| `NULL` (where allowed) | `nil` |
| enums | keywords, such as `:center` (integers also work) |
| flags | lists of keywords, such as `'(:expand :fill)` |
| `char **` (string arrays) | lists of strings |
| `guint8 *` buffers, `GByteArray` | octet vectors |
| other C arrays | lists (any sequence is accepted as input) |
| `GList`, `GSList`, `GPtrArray` | lists |
| `GHashTable` | `equal` hash tables (input may also be an alist) |
| objects | proxy objects, see the next chapter |
| structs such as `GdkRGBA` | struct objects with field accessors |
| `GValue` results | the Lisp value it holds |

## Lengths, out parameters and optional arguments

Array length parameters disappear: `g_base64_encode (data, len)` is `(glib:base64-encode data)`.

Out parameters become extra return values, after the C return value:

```
(gtk:widget-measure widget :horizontal -1)
;; => minimum, natural, minimum-baseline, natural-baseline

(multiple-value-bind (ok mirrored) (glib:unichar-get-mirror-char 40)
  ...)
```

When C fills in a struct you would allocate yourself, the Lisp function simply returns it:

```
(gtk:text-buffer-get-start-iter buffer)   ; => a gtk:text-iter
```

Trailing arguments that may be `NULL` are optional: `(gio:file-read-async file priority)` works without the cancellable and callback.

## Structs

Structs with public fields have accessors and a constructor:

```
(let ((color (gdk:make-rgba :red 1 :green 0.5 :alpha 1)))
  (setf (gdk:rgba-blue color) 0.25)
  (gdk:rgba-to-string color))
```

Struct values returned to Lisp are independent copies, freed automatically.

## Errors

Functions that report a `GError` signal a `glib:glib-error` condition instead:

```
(handler-case (gio:file-load-contents file nil)
  (glib:glib-error (e)
    (format t "~a (~a ~a)" (glib:glib-error-message e)
            (glib:glib-error-domain e) (glib:glib-error-code e))))
```

Each error domain and code also has its own condition class, such as `gio:io-error` and `gio:io-error-not-found`; see "The Lisp layer".
