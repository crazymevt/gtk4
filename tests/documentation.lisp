;;;; documentation.lisp — every binding documented and linked upstream

(in-package #:gtk4-tests)

(define-test documentation :parent gtk4-tests)

(defparameter *documented-packages*
  '(:glib :gobject :gmodule :gio :cairo :pango :pango-cairo :gdk-pixbuf :gdk :gsk :gtk
    :graphene :harfbuzz))

(defparameter *linked-packages*
  '(:glib :gobject :gmodule :gio :cairo :pango :pango-cairo :gdk-pixbuf :gdk :gsk :gtk)
  "Packages whose upstream docs have a page per function. Graphene and
HarfBuzz use other documentation layouts, so their bindings carry no link yet.")

(defun generated-functions (package)
  "External symbols of PACKAGE bound to generated functions (those with a C name)."
  (loop for s being the external-symbols of package
        when (and (fboundp s) (gtk4:c-name s)) collect s))

(define-test every-function-has-a-docstring :parent documentation
  (let ((missing '()) (count 0))
    (dolist (p *documented-packages*)
      (dolist (s (generated-functions p))
        (incf count)
        ;; Functions name their C function; property accessors (which may share
        ;; a symbol with a class, as gtk:entry-buffer does) link upstream.
        (let ((doc (documentation s 'function)))
          (unless (and doc (or (search "C: " doc) (search "See: " doc)))
            (push s missing)))))
    (true (> count 9000) "~d generated functions found" count)
    (is = 0 (length missing) "undocumented: ~s" (subseq missing 0 (min 10 (length missing))))))

(define-test every-function-links-upstream :parent documentation
  (let ((missing '()))
    (dolist (p *linked-packages*)
      (dolist (s (generated-functions p))
        (let ((url (gtk4:documentation-url s)))
          (unless (and url (eql 0 (search "https://" url)))
            (push s missing)))))
    (is = 0 (length missing) "without a link: ~s" (subseq missing 0 (min 10 (length missing))))))

(define-test no-raw-gi-docgen-markup :parent documentation
  ;; References like [method@Gtk.Widget.show] must be rewritten with Lisp names.
  (let ((raw '()))
    (dolist (p *documented-packages*)
      (dolist (s (generated-functions p))
        (let ((doc (documentation s 'function)))
          (when (and doc (some (lambda (kind) (search kind doc))
                               '("[method@" "[class@" "[func@" "[ctor@" "[property@" "[signal@"
                                 "[enum@" "[iface@" "[struct@")))
            (push s raw)))))
    (is = 0 (length raw) "unconverted markup in: ~s" (subseq raw 0 (min 10 (length raw))))))

(define-test lisp-and-c-names-round-trip :parent documentation
  (is eq 'gtk:widget-set-visible (gtk4:lisp-name "gtk_widget_set_visible"))
  (is string= "gtk_widget_set_visible" (gtk4:c-name 'gtk:widget-set-visible))
  (is eq 'gtk:button (gtk4:lisp-name "GtkButton"))
  (is eq 'gtk:align (gtk4:lisp-name "GtkAlign"))
  (is eq 'cairo:move-to (gtk4:lisp-name "cairo_move_to"))
  (is string= "https://docs.gtk.org/gtk4/class.Button.html" (gtk4:documentation-url 'gtk:button))
  (is string= "https://docs.gtk.org/gtk4/method.Widget.set_visible.html"
      (gtk4:documentation-url 'gtk:widget-set-visible))
  (true (search "cairographics.org/manual/cairo-Paths.html#cairo-move-to"
                (gtk4:documentation-url 'cairo:move-to))))

(define-test docstrings-use-lisp-names :parent documentation
  (let ((doc (documentation 'gtk:button-new-with-label 'function)))
    (true (search "`gtk:button`" doc))
    (true (search "Returns a `gtk:widget`." doc))
    (false (search "GtkButton" (subseq doc 0 (search "C: " doc))))))
