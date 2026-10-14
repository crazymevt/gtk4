;;;; docset.lisp — the reference as a docset, for the spec browser and Cadre
;;;;
;;;; The same material as the reference site (site.lisp), written as Lisp data
;;;; instead of HTML: one entry per class, interface, struct, enum, bound
;;;; function and property, plus the manual's chapters, written with the
;;;; docset library's WRITE-DOCSET. The format is described in docset's
;;;; docs/format.md.

(in-package #:gtk4.generator)

;;; Ids, and links from `gtk:widget-show` in converted docs to entries

(defvar *docset-ids* (make-hash-table :test 'equal)
  "Lisp reference (\"gtk:widget-show\") -> the id of its entry.")

(defun docset-id (kind ref)
  (format nil "~(~a~)/~a" kind ref))

(defun note-docset-id (ref id)
  (unless (gethash ref *docset-ids*)
    (setf (gethash ref *docset-ids*) id)))

(defun code-inline (text)
  (let ((id (gethash (string-downcase text) *docset-ids*)))
    (if id (list :ref id text) (list :code text))))

;;; Markdown to docset blocks, following MARKDOWN-TO-HTML

(defun markdown-inlines (text)
  "TEXT's `code`, **bold**, [links](url) and plain text as docset inlines."
  (let ((out '()) (plain (make-string-output-stream)) (i 0) (n (length text)))
    (flet ((flush ()
             (let ((s (get-output-stream-string plain)))
               (when (plusp (length s)) (push s out)))))
      (loop
        (when (>= i n) (return))
        (let ((c (char text i)))
          (cond
            ((and (char= c #\`) (position #\` text :start (1+ i)))
             (let ((end (position #\` text :start (1+ i))))
               (flush)
               (push (code-inline (subseq text (1+ i) end)) out)
               (setf i (1+ end))))
            ((and (char= c #\*) (< (1+ i) n) (char= (char text (1+ i)) #\*)
                  (search "**" text :start2 (+ i 2)))
             (let ((end (search "**" text :start2 (+ i 2))))
               (flush)
               (push (cons :b (markdown-inlines (subseq text (+ i 2) end))) out)
               (setf i (+ end 2))))
            ((and (char= c #\!) (< (1+ i) n) (char= (char text (1+ i)) #\[)
                  (search "](" text :start2 i) (position #\) text :start i))
             ;; Images are upstream files we do not ship; keep the alt text.
             (let ((mid (search "](" text :start2 i)))
               (write-string (subseq text (+ i 2) mid) plain)
               (setf i (1+ (position #\) text :start mid)))))
            ((and (char= c #\[) (search "](" text :start2 i)
                  (let ((mid (search "](" text :start2 i)))
                    (and (not (find #\] text :start (1+ i) :end mid))
                         (position #\) text :start mid))))
             (let* ((mid (search "](" text :start2 i))
                    (end (position #\) text :start mid)))
               (flush)
               (push (list* :link (resolve-link (subseq text (+ mid 2) end))
                            (markdown-inlines (subseq text (1+ i) mid)))
                     out)
               (setf i (1+ end))))
            (t (write-char c plain) (incf i)))))
      (flush))
    (nreverse out)))

(defun markdown-blocks (text &key (heading-offset 3))
  "TEXT's paragraphs, fenced code, bullet lists, tables and headings as docset blocks."
  (when text
    (let ((lines (uiop:split-string (strip-pictures text) :separator '(#\Newline)))
          (blocks '()) (para '()) (code nil) (code-language nil) (items nil) (table nil))
      (labels ((flush-para ()
                 (when para
                   (push (cons :p (markdown-inlines (format nil "~{~a~^ ~}" (reverse para)))) blocks)
                   (setf para '())))
               (close-list ()
                 (when items
                   (push (cons :list (mapcar (lambda (text) (list :item (cons :p (markdown-inlines text))))
                                             (reverse items)))
                         blocks)
                   (setf items nil)))
               (close-table ()
                 (when table
                   (let ((rows (reverse table)))
                     (push (list :table :header (first rows) :rows (rest rows)) blocks))
                   (setf table nil)))
               (cells (line)
                 (mapcar (lambda (c) (markdown-inlines (string-trim " " c)))
                         (butlast (rest (uiop:split-string line :separator '(#\|)))))))
        (dolist (raw lines)
          (let* ((line (string-right-trim '(#\Space #\Return) raw))
                 (trimmed (string-left-trim " " line)))
            (cond
              ((and (>= (length trimmed) 3) (string= "```" (subseq trimmed 0 3)))
               (flush-para) (close-list) (close-table)
               (if code
                   (progn (push (list :code-block code-language
                                      (format nil "~{~a~^~%~}" (reverse (rest code))))
                                blocks)
                          (setf code nil))
                   (setf code (list :open)
                         code-language (let ((lang (string-trim " " (subseq trimmed 3))))
                                         (and (plusp (length lang)) (string-downcase lang))))))
              (code (push line code))
              ((and (> (length line) 1) (char= (char line 0) #\|))
               (flush-para) (close-list)
               (let ((row (cells line)))
                 (unless (every (lambda (c) (every (lambda (x) (and (stringp x) (every (lambda (ch) (find ch "-: ")) x))) c))
                                row)
                   (push row table))))
              ((progn (close-table) nil))
              ((string= trimmed "") (flush-para) (close-list))
              ((and (> (length line) 1) (char= (char line 0) #\#))
               (flush-para) (close-list)
               (push (list* :heading (min 4 (+ heading-offset (or (position #\# line :test-not #'char=) 1)))
                            (markdown-inlines (string-trim "# " line)))
                     blocks))
              ((and (> (length trimmed) 2) (member (char trimmed 0) '(#\- #\*)) (char= (char trimmed 1) #\Space))
               (flush-para)
               (push (subseq trimmed 2) items))
              ((and items (char= (char raw 0) #\Space))
               ;; A continuation line of the last list item.
               (setf (first items) (format nil "~a ~a" (first items) trimmed)))
              (t (close-list) (push trimmed para)))))
        (flush-para) (close-list) (close-table)
        (when code
          (push (list :code-block code-language (format nil "~{~a~^~%~}" (reverse (rest code)))) blocks)))
      (nreverse blocks))))

(defun doc-blocks (doc)
  (markdown-blocks (convert-doc-text doc)))

(defun docset-summary (doc)
  "The first sentence of DOC, converted, in plain text."
  (let ((text (convert-doc-text (first-paragraph doc))))
    (when (and text (plusp (length text)))
      (let* ((text (remove #\` (substitute #\Space #\Newline text)))
             (end (search ". " text)))
        (if end (subseq text 0 (1+ end)) text)))))

(defun meta-paragraph (&rest parts)
  "A paragraph of PARTS, which are strings and inlines, separated by middle dots."
  (let ((parts (remove nil parts)))
    (when parts
      (list (cons :p (loop for (part . more) on parts
                           collect part
                           when more collect " · "))))))

;;; Entries

(defun entry (id names kind kind-name package sections &key parent children upstream summary)
  (append (list :entry id :names names :kind kind :kind-name kind-name)
          (when package (list :package package))
          (when parent (list :parent parent))
          (when children (list :children children))
          (when summary (list :summary summary))
          (when upstream (list :upstream upstream))
          (list :sections (remove nil sections :key #'cdr))))

(defun package-of (nsname)
  (string-upcase (namespace-package-name nsname)))

(defun callable-docset-entry (plan ns parent)
  (let* ((nsname (gir-namespace-name ns))
         (ref (lisp-ref (plan-symbol plan) nsname))
         (values (plan-values-description plan nsname)))
    (entry (docset-id (plan-kind plan) ref)
           (list (string-downcase (symbol-name (plan-symbol plan))))
           (plan-kind plan)
           (string-capitalize (substitute #\Space #\- (string (plan-kind plan))))
           (package-of nsname)
           (list (list* "Syntax"
                        (list :syntax :kind (plan-kind plan) :name (list ref)
                                      :args (let ((ll (plan-lambda-list plan))) (and (plusp (length ll)) (list ll))))
                        (append (when values (list (cons :p (markdown-inlines values))))
                                (when (plan-throws plan)
                                  (list (list :p "Signals " (code-inline "glib:glib-error") " on failure.")))))
                 (cons "Description" (doc-blocks (plan-doc plan)))
                 (cons "C" (meta-paragraph (list :code (plan-c-name plan))
                                           (and (plan-version plan) (format nil "since ~a" (plan-version plan)))
                                           (and (plan-deprecated plan) "deprecated"))))
           :parent parent
           :upstream (plan-url plan nsname)
           :summary (docset-summary (plan-doc plan)))))

(defun class-kind (item)
  (ecase (gir-class-kind item)
    (:class :class) (:interface :interface) ((:record :boxed) :struct) (:union :union)))

(defun type-ref (item nsname)
  (let ((sym (type-symbol (qualify (gir-item-name item) nsname))))
    (if sym (lisp-ref sym nsname) (qualify (gir-item-name item) nsname))))

(defun bound-callables (item)
  "ITEM's constructors, functions and methods that have plans."
  (remove-if-not (lambda (c) (gethash c *plans*))
                 (append (gir-class-constructors item) (gir-class-functions item) (gir-class-methods item))))

(defun property-docset-entry (p class ns)
  (let* ((nsname (gir-namespace-name ns))
         (sym (item-symbol p)))
    (entry (docset-id :property (lisp-ref sym nsname))
           (list (string-downcase (symbol-name sym)))
           :property "Property" (package-of nsname)
           (list (cons "Description"
                       (append (meta-paragraph (and (gir-property-readable p) "readable")
                                               (and (gir-property-writable p) "writable")
                                               (and (gir-property-construct-only p) "construct-only")
                                               (list :code (format nil ":~a" (gir-item-name p)))
                                               "initarg")
                               (doc-blocks (gir-item-doc p)))))
           :parent (docset-id (class-kind class) (type-ref class nsname))
           :upstream (doc-url nsname (format nil "property.~a.~a.html" (gir-item-name class) (gir-item-name p)))
           :summary (docset-summary (gir-item-doc p)))))

(defun class-docset-entries (ctx item ns)
  "The entry for class ITEM, then its callables' and properties' entries."
  (let* ((nsname (gir-namespace-name ns))
         (ref (type-ref item nsname))
         (id (docset-id (class-kind item) ref))
         (callables (bound-callables item))
         (properties (remove-if-not #'item-symbol (gir-class-properties item)))
         (layout (gethash (qualify (gir-item-name item) nsname) (context-layouts ctx))))
    (cons
     (entry id
            (remove nil (list (if (find #\: ref) (subseq ref (1+ (position #\: ref))) ref)
                              (gir-class-c-type item)))
            (class-kind item) (string-capitalize (string (class-kind item)))
            (package-of nsname)
            (list
             (cons "Description"
                   (append
                    (meta-paragraph (list :code (or (gir-class-c-type item) ref))
                                    (and (gir-class-abstract item) "abstract"))
                    (when (and (eq (gir-class-kind item) :class) (gir-class-parent item))
                      (let* ((pq (qualify (gir-class-parent item) nsname))
                             (psym (type-symbol pq)))
                        (list (list :p "Parent: " (if psym (code-inline (lisp-ref psym (namespace-of pq))) pq)))))
                    (doc-blocks (gir-item-doc item))))
             (when (and (layout-p layout) (layout-accessors layout))
               (cons "Fields"
                     (append
                      (when (layout-constructor layout)
                        (list (list :p "Construct with "
                                    (list :code (format nil "(~a &key~{ ~(~a~)~})"
                                                        (lisp-ref (layout-constructor layout) nsname)
                                                        (loop for a in (layout-accessors layout)
                                                              when (and (getf a :writable) (getf a :name))
                                                                collect (safe-variable-name (gir-item-name (getf a :field))))))
                                    ".")))
                      (list (cons :list
                                  (loop for a in (layout-accessors layout)
                                        when (getf a :name)
                                          collect (list* :item
                                                         (list :label (code-inline (lisp-ref (getf a :name) nsname)))
                                                         (cons :p (append (markdown-inlines (describe-spec (getf a :spec) nsname))
                                                                          (when (getf a :writable) (list " · writable with setf"))))
                                                         (doc-blocks (gir-item-doc (getf a :field))))))))))
             (when (gir-class-signals item)
               (cons "Signals"
                     (list (cons :list
                                 (loop for s in (gir-class-signals item)
                                       collect (list* :item
                                                      (list :label (list :code (format nil ":~a" (gir-item-name s))))
                                                      (list :code-block "lisp"
                                                            (format nil "(gobject:connect ~(~a~) :~a (lambda (~(~a~)~{ ~(~a~)~}) …))"
                                                                    (camel-to-kebab (gir-item-name item)) (gir-item-name s)
                                                                    (camel-to-kebab (gir-item-name item))
                                                                    (mapcar (lambda (p) (safe-variable-name (or (gir-parameter-name p) "arg")))
                                                                            (gir-callable-parameters s))))
                                                      (doc-blocks (gir-item-doc s))))))))
             (when (gir-class-virtual-methods item)
               (cons "Virtual functions"
                     (list (cons :list
                                 (loop for v in (gir-class-virtual-methods item)
                                       for vp = (gethash v *vfunc-plans*)
                                       for name = (snake-to-kebab (gir-item-name v))
                                       collect (list* :item
                                                      (list :label (list :code (format nil ":~a" name)))
                                                      (append
                                                       (if (vfunc-plan-p vp)
                                                           (list (list :code-block "lisp"
                                                                       (format nil "(gobject:define-vfunc (my-class :~a) (~a) …)"
                                                                               name (vfunc-lambda-list vp))))
                                                           (list (list :p (format nil "Not overridable from Lisp~@[: ~a~]."
                                                                                  (and (stringp vp) vp)))))
                                                       (doc-blocks (gir-item-doc v)))))))))
             (let ((plain (remove-if #'item-symbol (gir-class-properties item))))
               (when plain
                 (cons "Properties"
                       (list (cons :list
                                   (loop for p in plain
                                         collect (list* :item
                                                        (list :label (list :code (format nil ":~a" (gir-item-name p))))
                                                        (doc-blocks (gir-item-doc p))))))))))
            :children (append (loop for c in callables
                                    for plan = (gethash c *plans*)
                                    collect (docset-id (plan-kind plan) (lisp-ref (plan-symbol plan) nsname)))
                              (loop for p in properties
                                    collect (docset-id :property (lisp-ref (item-symbol p) nsname))))
            :upstream (doc-url nsname (type-fragment item))
            :summary (docset-summary (gir-item-doc item)))
     (append (loop for c in callables collect (callable-docset-entry (gethash c *plans*) ns id))
             (loop for p in properties collect (property-docset-entry p item ns))))))

(defun enum-docset-entry (item ns)
  (let* ((nsname (gir-namespace-name ns))
         (sym (type-symbol (qualify (gir-item-name item) nsname)))
         (ref (if sym (lisp-ref sym nsname) (qualify (gir-item-name item) nsname)))
         (flags (eq (gir-enum-kind item) :bitfield)))
    (entry (docset-id (if flags :flags :enum) ref)
           (remove nil (list (if sym (string-downcase (symbol-name sym)) ref) (gir-enum-c-type item)))
           (if flags :flags :enum) (if flags "Flags" "Enumeration")
           (package-of nsname)
           (list (cons "Description"
                       (append (meta-paragraph (list :code (or (gir-enum-c-type item) ref))
                                               (if flags "use a list of keywords" "a keyword"))
                               (doc-blocks (gir-item-doc item))))
                 (cons "Values"
                       (list (list :table
                                   :header '(("Keyword") ("Value") ("C") ("Description"))
                                   :rows (loop for m in (gir-enum-members item)
                                               collect (list (list (list :code (format nil ":~a" (snake-to-kebab (gir-member-name m)))))
                                                             (list (princ-to-string (gir-member-value m)))
                                                             (list (list :code (or (gir-member-c-identifier m) "")))
                                                             (markdown-inlines (or (convert-doc-text (first-paragraph (gir-member-doc m))) ""))))))))
           :upstream (doc-url nsname (type-fragment item))
           :summary (docset-summary (gir-item-doc item)))))

(defun manual-docset-entries ()
  (loop for (title name path) in (manual-pages)
        collect (let ((blocks (markdown-blocks (uiop:read-file-string path) :heading-offset 0)))
                  (entry (format nil "manual/~a" name) (list title) :page "Manual" nil
                         ;; The page's own # title is the entry's name.
                         (list (cons "" (if (eq (car (first blocks)) :heading) (rest blocks) blocks)))
                         :summary (let ((p (find :p blocks :key #'car)))
                                    (and p (format nil "~{~a~}" (remove-if-not #'stringp (cdr p)))))))))

;;; The docset

(defun note-namespace-ids (ns)
  (let ((nsname (gir-namespace-name ns)))
    (dolist (item (gir-namespace-classes ns))
      (let ((ref (type-ref item nsname)))
        (note-docset-id ref (docset-id (class-kind item) ref))))
    (dolist (item (gir-namespace-enums ns))
      (let ((ref (type-ref item nsname)))
        (note-docset-id ref (docset-id (if (eq (gir-enum-kind item) :bitfield) :flags :enum) ref))))
    (loop for (callable . nil) in (namespace-callables ns)
          for plan = (gethash callable *plans*)
          when plan
            do (let ((ref (lisp-ref (plan-symbol plan) nsname)))
                 (note-docset-id ref (docset-id (plan-kind plan) ref))))))

(defun namespace-toc (ns)
  (let ((nsname (gir-namespace-name ns)))
    (flet ((group (title items)
             (when items
               (list (list :title title
                           :children (loop for i in (sort (copy-list items) #'string< :key #'gir-item-name)
                                           collect (let ((ref (type-ref i nsname)))
                                                     (list :title (if (find #\: ref) (subseq ref (1+ (position #\: ref))) ref)
                                                           :entry (gethash ref *docset-ids*)))))))))
      (let ((classes (gir-namespace-classes ns))
            (functions (remove-if-not (lambda (f) (gethash f *plans*)) (gir-namespace-functions ns))))
        (list :title (format nil "~(~a~)" (namespace-package-name nsname))
              :children
              (append
               (group "Classes" (remove :class classes :key #'gir-class-kind :test-not #'eq))
               (group "Interfaces" (remove :interface classes :key #'gir-class-kind :test-not #'eq))
               (group "Structs" (remove-if-not (lambda (c) (member (gir-class-kind c) '(:record :boxed))) classes))
               (group "Unions" (remove :union classes :key #'gir-class-kind :test-not #'eq))
               (group "Enumerations" (remove :enumeration (gir-namespace-enums ns) :key #'gir-enum-kind :test-not #'eq))
               (group "Flags" (remove :bitfield (gir-namespace-enums ns) :key #'gir-enum-kind :test-not #'eq))
               (when functions
                 (list (list :title "Functions"
                             :children (loop for f in (sort (copy-list functions) #'string< :key #'gir-item-name)
                                             for plan = (gethash f *plans*)
                                             for ref = (lisp-ref (plan-symbol plan) nsname)
                                             collect (list :title (subseq ref (1+ (position #\: ref)))
                                                           :entry (docset-id (plan-kind plan) ref))))))))))))

(defun generate-docset (ctx directory)
  "Write the reference and the manual as a docset into DIRECTORY. Runs after the
binding files are written, which records the plans."
  (clrhash *docset-ids*)
  (dolist (ns (context-targets ctx)) (note-namespace-ids ns))
  (let ((entries (manual-docset-entries))
        (packages '()))
    (dolist (ns (context-targets ctx))
      (let ((nsname (gir-namespace-name ns))
            (*link-base* (cdr (assoc (gir-namespace-name ns) *doc-bases* :test #'string=))))
        (pushnew (package-of nsname) packages :test #'string=)
        (dolist (c (gir-namespace-classes ns))
          (setf entries (revappend (class-docset-entries ctx c ns) entries)))
        (dolist (e (gir-namespace-enums ns))
          (push (enum-docset-entry e ns) entries))
        (dolist (f (gir-namespace-functions ns))
          (let ((plan (gethash f *plans*)))
            (when plan (push (callable-docset-entry plan ns nil) entries))))))
    (setf entries (nreverse entries))
    (docset:write-docset directory
                  (list :id "gtk4"
                        :title "GTK 4"
                        :subtitle "GTK 4 bindings for SBCL, generated from GObject Introspection"
                        :version (asdf:component-version (asdf:find-system "gtk4"))
                        :system "gtk4"
                        :packages (nreverse packages)
                        :source "https://github.com/crazymevt/gtk4"
                        :attribution "Reference text from the GObject Introspection data of GLib, GObject, GIO, Pango, cairo, HarfBuzz, Graphene, GdkPixbuf, GDK, GSK, GTK and libadwaita, rewritten with Lisp names. Each entry links to its upstream page."
                        :license "The upstream documentation is under each project's license (LGPL-2.1-or-later for GTK and GLib)."
                        :toc (cons (list :title "Manual"
                                         :children (loop for e in entries
                                                         when (eq (getf (cddr e) :kind) :page)
                                                           collect (list :title (first (getf (cddr e) :names))
                                                                         :entry (second e))))
                                   (mapcar #'namespace-toc (context-targets ctx))))
                  entries)
    (format t "~&Docset: ~d entries in ~a~%" (length entries) directory)
    directory))
