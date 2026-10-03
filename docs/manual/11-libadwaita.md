# libadwaita

libadwaita provides GNOME's adaptive widgets and style: header bars, toolbar views, navigation and split views, preferences pages, toasts, dialogs, and the light and dark styles. It is an optional system, so programs that do not use it do not need the library:

```
(ql:quickload :gtk4-adwaita)
```

Everything libadwaita provides is in the `adw` package, named by the same rules as GTK (`adw_toast_new` is `adw:toast-new`, `AdwPreferencesPage` is `adw:preferences-page`), with docstrings linking to the [libadwaita reference](https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/). It needs libadwaita 1.5 or newer.

On macOS: `brew install libadwaita`. On Debian and Ubuntu: `apt install libadwaita-1-0`.

## Applications

Use `adw:application` instead of `gtk:application`; it initializes libadwaita and loads your app's `style.css` resource. `adw:run-application` creates one, calls your function when it activates, and runs it:

```
(defun activate (app)
  (gtk:window-present
   (gtk:build
     (adw:application-window :application app :title "Hello"
       (adw:toolbar-view
         (adw:header-bar :child-type "top")
         (adw:status-page :title "Hello" :description "From Lisp"))))))

(adw:run-application "org.example.Hello" 'activate)
```

`gtk:build` works with libadwaita widgets as with GTK's. Child types follow the libadwaita UI files: `"top"` and `"bottom"` in a toolbar view, `"start"` and `"end"` in a header bar.

## Light and dark styles

```
(setf (adw:color-scheme) :prefer-dark)   ; or :default, :force-light, :prefer-light, :force-dark
(adw:dark-p)                             ; is a dark style in use now?
```

## Toasts

```
(adw:show-toast overlay "Message deleted"
                :button-label "Undo" :on-button (lambda (toast) (restore-message)))
```

`overlay` is an `adw:toast-overlay` wrapping your content. `:timeout` sets how long the toast shows, in seconds; 0 keeps it until dismissed.

## Custom widgets

libadwaita classes can be subclassed like GTK's (see "Custom widgets"). A composite template's `parent` may be a libadwaita class, such as `AdwBin` or `AdwPreferencesPage`.

`examples/adwaita.lisp` puts these together: a preferences page in a toolbar view, toasts, and a light/dark switch.
