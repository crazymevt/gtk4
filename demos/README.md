# gtk4 demos

Demos of GTK 4 from Lisp, with a browser modelled on GTK's own `gtk4-demo`:
pick a demo, read its source, run it.

```
make demo
```

Each demo is one file in `demos/`, defined with `define-demo`. The demos are
written for this project on the same topics as upstream `gtk4-demo` (which is
LGPL-licensed); they are not translations of its code. Every demo is opened,
run and closed by the test suite (`tests/demos.lisp`).

| Category | Demo | Shows |
| --- | --- | --- |
| Basics | Hello world | A window, a button, a signal handler |
| Basics | Buttons | Push, toggle, check and radio buttons, switches |
| Layout | Grid layout | GtkGrid with spanning cells |
| Layout | Stack and sidebar | GtkStack, GtkStackSidebar, transitions |
| Layout | Overlay | GtkOverlay children over a text view |
| Input | Entries and numbers | Entry, password entry, spin button, scale |
| Input | Key events | GtkEventControllerKey |
| Lists | Filtered list | GtkListView, GtkStringList, GtkFilterListModel, expressions |
| Lists | Files in columns | GtkColumnView over GtkDirectoryList |
| Lists | Flow box | GtkFlowBox with drawn swatches |
| Text | Text view and tags | GtkTextView, GtkTextTag, selections |
| Theming | CSS basics | GtkCssProvider and CSS classes |
| Theming | Dark mode | GtkSettings properties |
| Drawing | Drawing area | GtkDrawingArea with cairo |
| Drawing | Paint | GtkGestureDrag, GtkGestureClick |
| UI definition | Builder | GtkBuilder from XML |
| Actions | Menus and actions | GMenu, GSimpleAction, stateful actions, shortcuts |
| Dialogs | Dialogs | GtkAlertDialog and GtkFileDialog (async) |
| Feedback | Progress and spinners | GtkProgressBar, GtkSpinner, GLib timeouts |
| Animation | Revealer | GtkRevealer transitions, GtkDropDown |
| Data exchange | Clipboard | GdkClipboard, async reads |

Demos that need widgets defined in Lisp (custom widgets, composite templates,
list item widgets as subclasses) come with M3, which adds GObject subclassing.
