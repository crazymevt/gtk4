;;;; site.lisp — the generated reference site
;;;;
;;;; One page per class, interface, record and enum, laid out like
;;;; docs.gtk.org (constructors, functions, methods, properties, signals),
;;;; each entry showing the Lisp form, what it returns, the converted
;;;; documentation, the C name and a link to the upstream page. The
;;;; hand-written manual in docs/manual/ is rendered alongside.

(in-package #:gtk4.generator)

;;; A small Markdown renderer: enough for GIR docs and the manual.

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for c across text
          do (case c
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defvar *link-base* nil
  "Upstream base URL that relative links in GIR docs resolve against.")

(defun resolve-link (url)
  (if (or (null *link-base*) (search "://" url) (eql (search "#" url) 0) (eql (search "mailto:" url) 0))
      url
      (concatenate 'string *link-base* url)))

(defun inline-markdown (text)
  "Escape TEXT and render `code`, **bold**, [text](url) and drop images."
  (let ((s (html-escape text)))
    (with-output-to-string (out)
      (let ((i 0) (n (length s)))
        (loop
          (when (>= i n) (return))
          (let ((c (char s i)))
            (cond
              ((and (char= c #\`) (position #\` s :start (1+ i)))
               (let ((end (position #\` s :start (1+ i))))
                 (format out "<code>~a</code>" (subseq s (1+ i) end))
                 (setf i (1+ end))))
              ((and (char= c #\*) (< (1+ i) n) (char= (char s (1+ i)) #\*)
                    (search "**" s :start2 (+ i 2)))
               (let ((end (search "**" s :start2 (+ i 2))))
                 (format out "<strong>~a</strong>" (subseq s (+ i 2) end))
                 (setf i (+ end 2))))
              ((and (char= c #\!) (< (1+ i) n) (char= (char s (1+ i)) #\[)
                    (search "](" s :start2 i) (position #\) s :start i))
               ;; Images refer to upstream files we do not ship; keep the alt text.
               (let ((mid (search "](" s :start2 i)))
                 (write-string (subseq s (+ i 2) mid) out)
                 (setf i (1+ (position #\) s :start mid)))))
              ((and (char= c #\[) (search "](" s :start2 i)
                    (let ((mid (search "](" s :start2 i)))
                      (and (not (find #\] s :start (1+ i) :end mid))
                           (position #\) s :start mid))))
               (let* ((mid (search "](" s :start2 i))
                      (end (position #\) s :start mid)))
                 (format out "<a href=\"~a\">~a</a>" (resolve-link (subseq s (+ mid 2) end))
                         (subseq s (1+ i) mid))
                 (setf i (1+ end))))
              (t (write-char c out) (incf i)))))))))

(defun strip-pictures (text)
  "Remove <picture>...</picture> blocks: they show upstream screenshots we do not ship."
  (loop for start = (search "<picture" text)
        for end = (and start (search "</picture>" text :start2 start))
        while (and start end)
        do (setf text (concatenate 'string (subseq text 0 start)
                                   (subseq text (+ end (length "</picture>"))))))
  text)

(defvar *heading-offset* 3
  "Added to Markdown heading levels: GIR prose headings sit below API entries;
manual pages use 0, so # is a page title.")

(defun markdown-to-html (text)
  "Render paragraphs, fenced code, bullet lists, tables and headings."
  (when text
    (setf text (strip-pictures text))
    (with-output-to-string (out)
      (let ((lines (uiop:split-string text :separator '(#\Newline)))
            (para '()) (in-code nil) (in-list nil) (in-table nil))
        (labels ((table-cells (line)
                   (mapcar (lambda (c) (string-trim " " c))
                           (butlast (rest (uiop:split-string line :separator '(#\|))))))
                 (close-table ()
                   (when in-table (format out "</table>~%") (setf in-table nil)))
                 (flush-para ()
                   (when para
                     (format out "<p>~a</p>~%" (inline-markdown (format nil "~{~a~^ ~}" (reverse para))))
                     (setf para '())))
                 (close-list ()
                   (when in-list (format out "</ul>~%") (setf in-list nil))))
          (dolist (raw lines)
            (let ((line (string-right-trim '(#\Space #\Return) raw)))
              (cond
                ((and (>= (length (string-left-trim " " line)) 3)
                      (string= "```" (subseq (string-left-trim " " line) 0 3)))
                 (flush-para) (close-list)
                 (if in-code
                     (progn (format out "</code></pre>~%") (setf in-code nil))
                     (progn (format out "<pre><code>") (setf in-code t))))
                (in-code (format out "~a~%" (html-escape line)))
                ;; Tables: | a | b |, with a | --- | separator after the header.
                ((and (> (length line) 1) (char= (char line 0) #\|))
                 (flush-para) (close-list)
                 (let ((cells (table-cells line)))
                   (cond ((every (lambda (c) (and (plusp (length c)) (every (lambda (ch) (find ch "-: ")) c)))
                                 cells))
                         ((not in-table)
                          (format out "<table><tr>~{<th>~a</th>~}</tr>~%" (mapcar #'inline-markdown cells))
                          (setf in-table t))
                         (t (format out "<tr>~{<td>~a</td>~}</tr>~%" (mapcar #'inline-markdown cells))))))
                ((progn (close-table) nil))
                ((string= (string-trim " " line) "") (flush-para) (close-list))
                ((and (> (length line) 1) (char= (char line 0) #\#))
                 (flush-para) (close-list)
                 (let* ((level (min 5 (+ *heading-offset* (or (position #\# line :test-not #'char=) 1))))
                        (title (string-trim "# " line)))
                   (format out "<h~d>~a</h~d>~%" level (inline-markdown title) level)))
                ((let ((trimmed (string-left-trim " " line)))
                   (and (> (length trimmed) 2) (member (char trimmed 0) '(#\- #\*))
                        (char= (char trimmed 1) #\Space)))
                 (flush-para)
                 (unless in-list (format out "<ul>~%") (setf in-list t))
                 (format out "<li>~a</li>~%" (inline-markdown (subseq (string-left-trim " " line) 2))))
                (t (when in-list
                     ;; A continuation line of the last list item.
                     (if (char= (char raw 0) #\Space)
                         (progn (format out "~a" (inline-markdown line)) (setf line nil))
                         (close-list)))
                   (when line (push (string-trim " " line) para))))))
          (flush-para) (close-list) (close-table)
          (when in-code (format out "</code></pre>~%")))))))

;;; Page scaffolding

(defparameter *site-css* "
:root { --bg:#fdfdfc; --fg:#1d1d1f; --muted:#5f6368; --line:#e3e3e0; --code:#f3f3f1; --accent:#2d5fb4; }
@media (prefers-color-scheme: dark) {
  :root { --bg:#16171a; --fg:#e6e6e6; --muted:#a0a4ab; --line:#2c2e33; --code:#202226; --accent:#7aa7ff; } }
* { box-sizing: border-box; }
body { margin:0; background:var(--bg); color:var(--fg); font:16px/1.55 system-ui, -apple-system, sans-serif; }
main { max-width: 860px; margin: 0 auto; padding: 24px 16px 64px; }
a { color: var(--accent); text-decoration: none; } a:hover { text-decoration: underline; }
nav { font-size: 14px; color: var(--muted); margin-bottom: 16px; }
h1 { font-size: 28px; margin: 8px 0 4px; } h2 { font-size: 20px; margin-top: 36px; border-bottom: 1px solid var(--line); padding-bottom: 4px; }
h3 { font-size: 16px; margin: 24px 0 4px; font-family: ui-monospace, Menlo, monospace; }
h4, h5 { font-size: 16px; margin: 20px 0 4px; } h5 { font-size: 15px; }
code, pre { font-family: ui-monospace, Menlo, monospace; font-size: 14px; background: var(--code); border-radius: 4px; }
code { padding: 1px 4px; } pre { padding: 12px; overflow-x: auto; } pre code { padding: 0; background: none; }
.meta { color: var(--muted); font-size: 14px; } .meta a { margin-left: 4px; }
.sig { background: var(--code); padding: 8px 12px; border-radius: 6px; font-family: ui-monospace, Menlo, monospace; font-size: 14px; overflow-x: auto; }
ul.index { columns: 2; padding-left: 18px; } @media (max-width: 600px) { ul.index { columns: 1; } }
table { border-collapse: collapse; width: 100%; font-size: 14px; } td, th { border-bottom: 1px solid var(--line); padding: 6px 8px; text-align: left; vertical-align: top; }
")

(defun write-page (path title body &key (depth 1))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (format out "<!doctype html>~%<html lang=\"en\"><head><meta charset=\"utf-8\">~%")
    (format out "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
    (format out "<title>~a</title><link rel=\"stylesheet\" href=\"~astyle.css\"></head>~%"
            (html-escape title) (relative-root depth))
    (format out "<body><main>~%<nav><a href=\"~aindex.html\">gtk4 for SBCL</a></nav>~%~a~%</main></body></html>~%"
            (relative-root depth) body)))

(defun relative-root (depth)
  (format nil "~v@{../~:*~}" depth nil))

(defun upstream-link (url)
  (if url (format nil " · <a href=\"~a\">upstream docs</a>" url) ""))

(defun symbol-html (symbol nsname)
  (html-escape (lisp-ref symbol nsname)))

(defun type-page-name (item)
  (type-fragment item))

;;; Entries

(defun callable-entry-html (callable owner ns)
  (declare (ignore owner))
  (let* ((nsname (gir-namespace-name ns))
         (plan (gethash callable *plans*)))
    (if (null plan)
        (format nil "<h3>~a</h3><p class=\"meta\">Not bound yet (C: <code>~a</code>).</p>"
                (html-escape (gir-item-name callable))
                (html-escape (or (gir-callable-c-identifier callable) "")))
        (let ((url (plan-url plan nsname)))
          (with-output-to-string (out)
            (format out "<h3 id=\"~(~a~)\">~a</h3>~%" (symbol-name (plan-symbol plan))
                    (symbol-html (plan-symbol plan) nsname))
            (format out "<div class=\"sig\">(~a~@[ ~a~])</div>~%"
                    (symbol-html (plan-symbol plan) nsname)
                    (let ((ll (plan-lambda-list plan))) (and (plusp (length ll)) (html-escape ll))))
            (let ((values (plan-values-description plan nsname)))
              (when values (format out "<p>~a</p>~%" (inline-markdown values))))
            (when (plan-throws plan)
              (format out "<p>Signals <code>glib:glib-error</code> on failure.</p>~%"))
            (format out "~a" (or (markdown-to-html (convert-doc-text (plan-doc plan))) ""))
            (format out "<p class=\"meta\">C: <code>~a</code>~a~@[ · since ~a~]~:[~; · deprecated~]</p>~%"
                    (html-escape (plan-c-name plan)) (upstream-link url)
                    (plan-version plan) (plan-deprecated plan)))))))

(defun property-entry-html (p class nsname)
  (let ((sym (item-symbol p)))
    (format nil "<h3>~a</h3>~%<p class=\"meta\">~:[~;readable~]~:[~; · writable~]~:[~; · construct-only~]~@[ · <code>~a</code>~]~a</p>~%~a"
            (if sym (symbol-html sym nsname) (html-escape (gir-item-name p)))
            (gir-property-readable p) (gir-property-writable p) (gir-property-construct-only p)
            (and sym (format nil ":~a initarg" (gir-item-name p)))
            (upstream-link (doc-url nsname (format nil "property.~a.~a.html" (gir-item-name class)
                                                   (gir-item-name p))))
            (or (markdown-to-html (convert-doc-text (gir-item-doc p))) ""))))

(defun signal-entry-html (s class nsname)
  (format nil "<h3>:~a</h3>~%<div class=\"sig\">(gobject:connect ~(~a~) :~a (lambda (~(~a~)~{ ~(~a~)~}) …))</div>~%~a<p class=\"meta\">~a</p>~%"
          (html-escape (gir-item-name s))
          (camel-to-kebab (gir-item-name class)) (html-escape (gir-item-name s))
          (camel-to-kebab (gir-item-name class))
          (mapcar (lambda (p) (safe-variable-name (or (gir-parameter-name p) "arg")))
                  (gir-callable-parameters s))
          (or (markdown-to-html (convert-doc-text (gir-item-doc s))) "")
          (upstream-link (doc-url nsname (format nil "signal.~a.~a.html" (gir-item-name class)
                                                 (gir-item-name s))))))

;;; Pages

(defun class-page-html (ctx item ns)
  (let* ((nsname (gir-namespace-name ns))
         (qualified (qualify (gir-item-name item) nsname))
         (sym (type-symbol qualified))
         (url (doc-url nsname (type-fragment item))))
    (with-output-to-string (out)
      (format out "<h1>~a</h1>~%<p class=\"meta\">~(~a~) · C: <code>~a</code>~a</p>~%"
              (if sym (symbol-html sym nsname) (html-escape qualified))
              (gir-class-kind item) (html-escape (or (gir-class-c-type item) ""))
              (upstream-link url))
      (when (and (eq (gir-class-kind item) :class) (gir-class-parent item))
        (let* ((pq (qualify (gir-class-parent item) nsname))
               (pi* (lookup ctx pq)))
          (format out "<p>Parent: ~a</p>~%"
                  (if (and pi* (type-symbol pq))
                      (format nil "<a href=\"../~(~a~)/~a\">~a</a>"
                              (namespace-package-name (namespace-of pq)) (type-page-name pi*)
                              (symbol-html (type-symbol pq) (namespace-of pq)))
                      (html-escape pq)))))
      (format out "~a" (or (markdown-to-html (convert-doc-text (gir-item-doc item))) ""))
      (flet ((section (title items render)
               (when items
                 (format out "<h2>~a</h2>~%" title)
                 (dolist (i items) (format out "~a~%" (funcall render i))))))
        (section "Constructors" (gir-class-constructors item)
                 (lambda (c) (callable-entry-html c item ns)))
        (section "Functions" (gir-class-functions item)
                 (lambda (c) (callable-entry-html c item ns)))
        (section "Methods" (gir-class-methods item)
                 (lambda (c) (callable-entry-html c item ns)))
        (let ((layout (gethash qualified (context-layouts ctx))))
          (when (and (layout-p layout) (layout-accessors layout))
            (format out "<h2>Fields</h2>~%")
            (when (layout-constructor layout)
              (format out "<p>Construct with <code>(~a &amp;key~{ ~(~a~)~})</code>.</p>~%"
                      (symbol-html (layout-constructor layout) nsname)
                      (loop for a in (layout-accessors layout)
                            when (and (getf a :writable) (getf a :name))
                              collect (safe-variable-name (gir-item-name (getf a :field))))))
            (dolist (a (layout-accessors layout))
              (when (getf a :name)
                (format out "<h3>~a</h3>~%<p class=\"meta\">~a~:[~; · writable with setf~]</p>~a~%"
                        (symbol-html (getf a :name) nsname)
                        (inline-markdown (describe-spec (getf a :spec) nsname))
                        (getf a :writable)
                        (or (markdown-to-html (convert-doc-text (gir-item-doc (getf a :field)))) ""))))))
        (section "Properties" (gir-class-properties item)
                 (lambda (p) (property-entry-html p item nsname)))
        (section "Signals" (gir-class-signals item)
                 (lambda (s) (signal-entry-html s item nsname)))))))

(defun enum-page-html (item ns)
  (let* ((nsname (gir-namespace-name ns))
         (sym (type-symbol (qualify (gir-item-name item) nsname))))
    (with-output-to-string (out)
      (format out "<h1>~a</h1>~%<p class=\"meta\">~:[enumeration~;flags (use a list of keywords)~] · C: <code>~a</code>~a</p>~%"
              (if sym (symbol-html sym nsname) (html-escape (gir-item-name item)))
              (eq (gir-enum-kind item) :bitfield)
              (html-escape (or (gir-enum-c-type item) ""))
              (upstream-link (doc-url nsname (type-fragment item))))
      (format out "~a" (or (markdown-to-html (convert-doc-text (gir-item-doc item))) ""))
      (format out "<h2>Values</h2>~%<table><tr><th>Keyword</th><th>Value</th><th>C</th><th></th></tr>~%")
      (dolist (m (gir-enum-members item))
        (format out "<tr><td><code>:~a</code></td><td>~a</td><td><code>~a</code></td><td>~a</td></tr>~%"
                (html-escape (snake-to-kebab (gir-member-name m))) (gir-member-value m)
                (html-escape (or (gir-member-c-identifier m) ""))
                (inline-markdown (or (convert-doc-text (first-paragraph (gir-member-doc m))) ""))))
      (format out "</table>~%"))))

(defun namespace-index-html (ns)
  (let ((nsname (gir-namespace-name ns)))
    (with-output-to-string (out)
      (format out "<h1>~(~a~)</h1>~%<p class=\"meta\">GIR namespace ~a ~a~a</p>~%"
              (namespace-package-name nsname) nsname (gir-namespace-version ns)
              (let ((base (cdr (assoc nsname *doc-bases* :test #'string=))))
                (if base (format nil " · <a href=\"~a\">upstream docs</a>" base) "")))
      (flet ((listing (title items)
               (when items
                 (format out "<h2>~a</h2>~%<ul class=\"index\">~%" title)
                 (dolist (i (sort (copy-list items) #'string< :key #'gir-item-name))
                   (let ((sym (type-symbol (qualify (gir-item-name i) nsname))))
                     (format out "<li><a href=\"~a\">~a</a></li>~%" (type-page-name i)
                             (if sym (html-escape (string-downcase (symbol-name sym)))
                                 (html-escape (gir-item-name i))))))
                 (format out "</ul>~%"))))
        (let ((classes (gir-namespace-classes ns)))
          (listing "Classes" (remove :class classes :key #'gir-class-kind :test-not #'eq))
          (listing "Interfaces" (remove :interface classes :key #'gir-class-kind :test-not #'eq))
          (listing "Structs" (remove-if-not (lambda (c) (member (gir-class-kind c) '(:record :boxed))) classes))
          (listing "Unions" (remove :union classes :key #'gir-class-kind :test-not #'eq)))
        (listing "Enumerations" (remove :enumeration (gir-namespace-enums ns) :key #'gir-enum-kind :test-not #'eq))
        (listing "Flags" (remove :bitfield (gir-namespace-enums ns) :key #'gir-enum-kind :test-not #'eq))
        (when (gir-namespace-functions ns)
          (format out "<h2>Functions</h2>~%<p><a href=\"functions.html\">~d functions</a></p>~%"
                  (length (gir-namespace-functions ns))))))))

(defun functions-page-html (ns)
  (with-output-to-string (out)
    (format out "<h1>~(~a~) functions</h1>~%" (namespace-package-name (gir-namespace-name ns)))
    (dolist (f (sort (copy-list (gir-namespace-functions ns)) #'string< :key #'gir-item-name))
      (format out "~a~%" (callable-entry-html f nil ns)))))

(defun manual-pages ()
  "(TITLE NAME PATH) for each docs/manual/*.md, ordered by file name."
  (let ((dir (asdf:system-relative-pathname "gtk4" "docs/manual/")))
    (loop for path in (sort (directory (merge-pathnames "*.md" dir)) #'string< :key #'namestring)
          collect (let* ((text (uiop:read-file-string path))
                         (first-line (subseq text 0 (or (position #\Newline text) (length text)))))
                    (list (string-trim "# " first-line) (pathname-name path) path)))))

(defun generate-site (ctx directory)
  "Write the reference site and manual into DIRECTORY."
  (ensure-directories-exist directory)
  (with-open-file (out (merge-pathnames "style.css" directory) :direction :output :if-exists :supersede)
    (write-string *site-css* out))
  ;; Front page
  (write-page (merge-pathnames "index.html" directory) "gtk4 for SBCL"
              (with-output-to-string (out)
                (format out "<h1>gtk4 for SBCL</h1>~%<p>Complete GTK 4 bindings for SBCL, generated from GObject Introspection. Every entry links to its upstream page; names follow simple rules described in the manual.</p>~%")
                (format out "<h2>Manual</h2>~%<ul>~%")
                (loop for (title name) in (manual-pages)
                      do (format out "<li><a href=\"manual/~a.html\">~a</a></li>~%" name (html-escape title)))
                (format out "</ul>~%<h2>Reference</h2>~%<ul class=\"index\">~%")
                (dolist (ns (context-targets ctx))
                  (format out "<li><a href=\"~(~a~)/index.html\">~(~a~)</a> <span class=\"meta\">~a</span></li>~%"
                          (namespace-package-name (gir-namespace-name ns))
                          (namespace-package-name (gir-namespace-name ns))
                          (gir-namespace-name ns)))
                (format out "</ul>~%"))
              :depth 0)
  ;; Manual
  (loop for (title name path) in (manual-pages)
        do (write-page (merge-pathnames (format nil "manual/~a.html" name) directory) title
                       (let ((*heading-offset* 0))
                         (markdown-to-html (uiop:read-file-string path)))))
  ;; Reference
  (dolist (ns (context-targets ctx))
    (let ((dir (merge-pathnames (format nil "~(~a~)/" (namespace-package-name (gir-namespace-name ns)))
                                directory))
          (*link-base* (cdr (assoc (gir-namespace-name ns) *doc-bases* :test #'string=))))
      (write-page (merge-pathnames "index.html" dir) (gir-namespace-name ns) (namespace-index-html ns))
      (write-page (merge-pathnames "functions.html" dir) (format nil "~a functions" (gir-namespace-name ns))
                  (functions-page-html ns))
      (dolist (c (gir-namespace-classes ns))
        (write-page (merge-pathnames (type-page-name c) dir) (gir-item-name c) (class-page-html ctx c ns)))
      (dolist (e (gir-namespace-enums ns))
        (write-page (merge-pathnames (type-page-name e) dir) (gir-item-name e) (enum-page-html e ns)))))
  directory)
