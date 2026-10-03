# Tutorial: a text editor

This tutorial builds a small text editor, one step at a time: a window with a text view, a live word count, opening and saving files, error messages, and finally a standalone program. The finished code is `examples/editor.lisp`; run it with `make example NAME=editor`.

You need gtk4 installed and loading (see the introduction). Type each step at a REPL, or put the code in a file and load it.

## 1. A window

Every GTK program is a `gtk:application`. When it starts, it emits `activate`, and that is where the program opens its window:

```
(defpackage #:editor (:use #:cl))
(in-package #:editor)

(defun activate (app)
  (let ((window (make-instance 'gtk:application-window :application app
                               :title "Editor" :default-width 600 :default-height 420)))
    (gtk:window-present window)))

(defun main ()
  (let ((app (gtk:application-new "org.example.Editor" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (gio:application-run app nil)))
```

Run `(editor:main)` on the main thread. Run it from the terminal (`sbcl --load editor.lisp --eval '(editor:main)'`), or see "Threads and macOS" for using an editor's REPL. Closing the window ends `main`.

Two things to notice. `make-instance` takes GObject properties as initargs, here `:title` and `:default-width`. And `activate` is connected by symbol, so redefining the function changes what the next activation does.

## 2. A text view and a status line

`gtk:build` creates a widget tree from one form. Replace `activate` with a version that fills the window:

```
(defun activate (app)
  (multiple-value-bind (window ids)
      (gtk:build
        (gtk:application-window :application app :title "Editor"
                                :default-width 600 :default-height 420
          (gtk:box :orientation :vertical
            (gtk:scrolled-window :vexpand t
              (gtk:text-view :id :text :monospace t :wrap-mode :word-char
                             :left-margin 12 :right-margin 12 :top-margin 12))
            (gtk:label :id :status :xalign 0.0 :margin-start 12 :margin-top 6 :margin-bottom 6))))
    (declare (ignorable ids))
    (gtk:window-present window)))
```

Each node is a class followed by initargs and children. `:id` names an object so you can find it again: `gtk:build` returns the root and a table of ids, and `(gethash :text ids)` is the text view.

## 3. Counting words

A text view shows a `gtk:text-buffer`, which emits `changed` after every edit. Read the text between the buffer's start and end, count the words, and show the result:

```
(defun buffer-text (buffer)
  (multiple-value-bind (start end) (gtk:text-buffer-get-bounds buffer)
    (gtk:text-buffer-get-text buffer start end nil)))

(defun word-count (text)
  (length (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline))
                  :test #'string=)))

(defun show-status (buffer label)
  (let ((text (buffer-text buffer)))
    (gtk:label-set-text label (format nil "~:d word~:p, ~:d character~:p"
                                      (word-count text) (length text)))))
```

`gtk:text-buffer-get-bounds` fills in two `GtkTextIter` structures in C; Lisp returns them as two values. Connect the handler in `activate`, before presenting the window:

```
(let ((buffer (gtk:text-view-get-buffer (gethash :text ids)))
      (label (gethash :status ids)))
  (gobject:connect buffer :changed (lambda (b) (show-status b label)))
  (show-status buffer label))
```

The handler receives the object that emitted the signal, followed by the signal's arguments; `changed` has none. Because the closure calls `show-status` by name, you can redefine `show-status` while the editor runs, to also count lines, say, and the next keystroke uses the new definition.

## 4. A window class of our own

The window now has state: the buffer, the status label, and soon the file being edited. A Lisp class can hold it, and the class can be a real GTK window class:

```
(defclass editor-window (gtk:application-window)
  ((buffer :reader editor-buffer)
   (status :reader editor-status)
   (file :initform nil :accessor editor-file))
  (:metaclass gobject:gobject-class)
  (:gtype-name "EditorWindow"))
```

The metaclass and `:gtype-name` make GTK register a new GType, `EditorWindow`, the first time the class is used. An `initialize-instance :after` method builds the contents, as `activate` did; `examples/editor.lisp` shows the whole method. Signal handlers can then find their window from any widget in it, with `gtk:widget-get-root`, rather than capturing it.

## 5. Opening a file

Opening a file takes two asynchronous steps: the file dialog, then reading the file. In C each is a pair of functions, `gtk_file_dialog_open` and `gtk_file_dialog_open_finish`, joined by a callback. `gio:async` joins them in Lisp:

```
(defun open-clicked (button)
  (let ((window (gtk:widget-get-root button)))
    (gio:async (gtk:file-dialog-open (gtk:file-dialog-new) window)
               (lambda (file) (load-file window file))
               :error (lambda (e) (report-error window e)))))

(defun load-file (window file)
  (gio:async (gio:file-load-contents-async file)
             (lambda (ok contents etag)
               (declare (ignore ok etag))
               (setf (editor-file window) file)
               (gtk:text-buffer-set-text
                (editor-buffer window)
                (sb-ext:octets-to-string (coerce contents '(vector (unsigned-byte 8)))
                                         :external-format :utf-8)
                -1))
             :error (lambda (e) (report-error window e))))
```

The first function receives the values of the `_finish` function: the chosen file, and then the contents. The program keeps running meanwhile; the window stays responsive while the dialog is open or a large file loads.

Add an Open button to a header bar in the window's `initialize-instance :after` method:

```
(gtk:window-set-titlebar
 window (gtk:build
          (gtk:header-bar
            (gtk:button :label "Open" :child-type "start" :on-clicked 'open-clicked))))
```

`:on-clicked` connects the `clicked` signal, and `:child-type "start"` packs the button at the header bar's start, as `<child type="start">` does in a GtkBuilder file.

## 6. Errors

When the user closes the file dialog, or a file cannot be read, the `_finish` function reports a `GError`. In Lisp that is a condition: a subclass of `glib:glib-error`, with a class for each error domain and code. Ignore the dialog being dismissed, and show anything else:

```
(defun report-error (window condition)
  (unless (typep condition 'gtk:dialog-error-dismissed)
    (gtk:alert-dialog-show (make-instance 'gtk:alert-dialog
                                          :message "Something went wrong"
                                          :detail (glib:glib-error-message condition))
                           window)))
```

`gtk_alert_dialog_new` takes printf-style arguments, which no binding can express, so the alert is made with `make-instance` and its properties instead. The reference lists, for every function, whether it is bound and why not.

## 7. Saving

Saving mirrors opening, with `gtk:file-dialog-save` when the text has no file yet and `gio:file-replace-contents-async` to write it:

```
(defun save-file (window file)
  (let ((octets (sb-ext:string-to-octets (buffer-text (editor-buffer window))
                                         :external-format :utf-8)))
    (gio:async (gio:file-replace-contents-async file octets nil nil '(:none))
               (lambda (ok etag)
                 (declare (ignore ok etag))
                 (setf (editor-file window) file))
               :error (lambda (e) (report-error window e)))))
```

The arguments follow the C function: the contents, an entity tag (none), whether to keep a backup, and flags.

## 8. Working at the REPL

Everything above can change while the editor runs. Redefine `word-count`, `show-status` or `report-error` and the running program uses the new version, because handlers are connected by symbol or call these functions by name. You can also redefine `editor-window` with a new slot, and existing windows gain it. "Custom widgets" covers what can and cannot change in a class that GTK has already registered.

On macOS, GTK must run on the program's first thread, while editors run the REPL in another one. "Threads and macOS" shows the setup: start a Swank or Slynk server, then run `main`. Code evaluated from the REPL that calls GTK directly goes through `glib:in-main-thread`.

## 9. Shipping it

`gtk4:save-executable` saves the program as one executable file:

```
sbcl --non-interactive --load editor.lisp \
     --eval '(gtk4:save-executable "editor" (lambda () (editor:main)))'
```

It runs on machines with GTK installed. On macOS, `make app NAME=editor APP=Editor` builds an application bundle carrying its own copy of GTK (see `scripts/macos-app.sh`). On Linux, `packaging/flatpak/` shows how to build a Flatpak.

## Where next

- "Custom widgets": drawing with `:snapshot`, Lisp-defined properties and signals, composite templates.
- "The Lisp layer": list views of Lisp values, CSS from s-expressions.
- The demo browser, `make demo`: over twenty small programs to read and run.
- The reference: every function, with a link to GTK's documentation.
