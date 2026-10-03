;;;; list-filter.lisp — a filtered list view
;;;; GTK docs: https://docs.gtk.org/gtk4/section-list-widget.html

(in-package #:gtk4-demo)

(defparameter *color-names*
  '("alice blue" "aqua" "azure" "beige" "bisque" "black" "blue" "brown" "chartreuse"
    "chocolate" "coral" "crimson" "cyan" "fuchsia" "gold" "goldenrod" "gray" "green"
    "honeydew" "indigo" "ivory" "khaki" "lavender" "lime" "linen" "magenta" "maroon"
    "navy" "olive" "orange" "orchid" "peru" "pink" "plum" "purple" "red" "salmon"
    "sienna" "silver" "snow" "tan" "teal" "thistle" "tomato" "turquoise" "violet"
    "wheat" "white" "yellow"))

(define-demo list-filter
    (:title "Filtered list"
     :category "Lists"
     :description "A GtkListView over a string list, filtered as you type into the search
entry. The model chain is GtkStringList, GtkFilterListModel with a GtkStringFilter, then a
GtkNoSelection.")
  (let* ((window (make-demo-frame "Filtered list" :width 320 :height 480))
         (search (gtk:search-entry-new))
         (strings (gtk:string-list-new *color-names*))
         ;; The filter compares each item's "string" property with the search text.
         (filter (gtk:string-filter-new
                  (gtk:property-expression-new (gobject:class-gtype 'gtk:string-object) nil "string")))
         (filtered (gtk:filter-list-model-new strings filter))
         (view (gtk:list-view-new (gtk:no-selection-new filtered)
                                  (label-list-factory #'gtk:string-object-get-string)))
         (count (gtk:label-new "")))
    (flet ((show-count ()
             (gtk:label-set-text count (format nil "~d of ~d colors"
                                               (gio:list-model-get-n-items filtered)
                                               (length *color-names*)))))
      (gobject:connect search :search-changed
                       (lambda (entry)
                         (gtk:string-filter-set-search filter (gtk:editable-get-text entry))
                         (show-count)))
      (show-count))
    (gtk:window-set-child window (margins (vbox 6 search (scrolled view) count) 12))
    window))
