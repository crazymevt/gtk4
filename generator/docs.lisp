;;;; docs.lisp — GTK's documentation markup rewritten with Lisp names
;;;;
;;;; GIR doc strings use gi-docgen markup: [method@Gtk.Widget.show],
;;;; [property@Gtk.Widget:visible], gtk_widget_show(), %TRUE, @param.
;;;; DOC-INDEX maps every such reference to its Lisp name, so converted text
;;;; reads naturally next to the Lisp API while staying one-to-one with the
;;;; upstream pages.

(in-package #:gtk4.generator)

(defvar *doc-index* (make-hash-table :test 'equal)
  "Reference -> its Lisp rendering. Keys are gi-docgen targets (\"Gtk.Widget\",
\"Gtk.Widget.show\", \"Gtk.Widget:visible\", \"Gtk.Button::clicked\") and C
identifiers (\"gtk_widget_show\", \"GtkWidget\", \"GTK_ALIGN_CENTER\").")

(defvar *plans* (make-hash-table :test 'eq)
  "Callable -> its PLAN, recorded while emitting, for the reference site.")

(defun lisp-ref (symbol nsname)
  "SYMBOL written with its namespace's package prefix, e.g. gtk:widget-show."
  (format nil "~(~a:~a~)" (namespace-package-name nsname) (symbol-name symbol)))

(defun index-doc-name (key value)
  (when (and key value)
    (unless (gethash key *doc-index*)
      (setf (gethash key *doc-index*) value))))

(defun build-doc-index (ctx)
  "Fill *DOC-INDEX* from the names claimed in phase 1."
  (clrhash *doc-index*)
  (dolist (ns (context-targets ctx))
    (let ((n (gir-namespace-name ns)))
      (flet ((ref (symbol) (and symbol (lisp-ref symbol n))))
        ;; Types
        (dolist (item (append (gir-namespace-classes ns) (gir-namespace-enums ns)
                              (gir-namespace-callbacks ns) (gir-namespace-aliases ns)))
          (let* ((qualified (qualify (gir-item-name item) n))
                 (r (ref (type-symbol qualified))))
            (index-doc-name qualified r)
            (index-doc-name (typecase item
                              (gir-class (gir-class-c-type item))
                              (gir-enum (gir-enum-c-type item))
                              (gir-alias (gir-alias-c-type item)))
                            r)))
        ;; Enum members become keywords.
        (dolist (e (gir-namespace-enums ns))
          (dolist (m (gir-enum-members e))
            (let ((kw (format nil ":~a" (snake-to-kebab (gir-member-name m)))))
              (index-doc-name (gir-member-c-identifier m) kw)
              (index-doc-name (format nil "~a.~a.~a" n (gir-item-name e)
                                      (string-upcase (gir-member-name m)))
                              kw)
              (index-doc-name (format nil "~a.~a.~a" n (gir-item-name e) (gir-member-name m))
                              kw))))
        ;; Constants
        (dolist (c (gir-namespace-constants ns))
          (let ((r (ref (item-symbol c))))
            (index-doc-name (qualify (gir-item-name c) n) r)
            (index-doc-name (gir-constant-c-type c) r)))
        ;; Functions
        (dolist (f (gir-namespace-functions ns))
          (let ((r (ref (item-symbol f))))
            (index-doc-name (format nil "~a.~a" n (gir-item-name f)) r)
            (index-doc-name (gir-callable-c-identifier f) r)))
        ;; Members of classes, interfaces and records
        (dolist (c (gir-namespace-classes ns))
          (let ((owner (format nil "~a.~a" n (gir-item-name c))))
            (dolist (f (append (gir-class-constructors c) (gir-class-functions c)
                               (gir-class-methods c)))
              (let ((r (ref (item-symbol f))))
                (index-doc-name (format nil "~a.~a" owner (gir-item-name f)) r)
                (index-doc-name (gir-callable-c-identifier f) r)))
            (dolist (p (gir-class-properties c))
              (index-doc-name (format nil "~a:~a" owner (gir-item-name p)) (ref (item-symbol p))))
            (dolist (s (gir-class-signals c))
              (index-doc-name (format nil "~a::~a" owner (gir-item-name s))
                              (format nil ":~a" (gir-item-name s))))
            (dolist (v (gir-class-virtual-methods c))
              (index-doc-name (format nil "~a.~a" owner (gir-item-name v))
                              (format nil "%vfunc ~a" (snake-to-kebab (gir-item-name v)))))))))))

;;; Converting text

(defun identifier-char-p (c) (or (alphanumericp c) (char= c #\_)))

(defun scan-identifier (text start)
  "End of the identifier starting at START."
  (or (position-if-not #'identifier-char-p text :start start) (length text)))

(defun render-ref (key fallback)
  "The Lisp rendering of KEY, in backquotes, or FALLBACK."
  (let ((r (gethash key *doc-index*)))
    (cond ((null r) fallback)
          ((char= (char r 0) #\:) r)              ; a keyword, or a :signal
          ((eql (search "%vfunc " r) 0) (format nil "`~a`" (subseq r 7)))
          (t (format nil "`~a`" r)))))

(defun convert-doc-text (text)
  "TEXT with gi-docgen markup and C names rewritten using *DOC-INDEX*."
  (when text
    (with-output-to-string (out)
      (let ((i 0) (n (length text)))
        (loop
          (when (>= i n) (return))
          (let ((c (char text i)))
            (cond
              ;; [text][kind@Target] -> text
              ((and (char= c #\[)
                    (let ((close (position #\] text :start i)))
                      (and close (< (1+ close) n) (char= (char text (1+ close)) #\[)
                           (let ((close2 (position #\] text :start (1+ close))))
                             (and close2 (find #\@ text :start close :end close2))))))
               (let* ((close (position #\] text :start i))
                      (close2 (position #\] text :start (1+ close))))
                 (write-string (subseq text (1+ i) close) out)
                 (setf i (1+ close2))))
              ;; [kind@Target]
              ((and (char= c #\[)
                    (let ((at (position #\@ text :start i))
                          (close (position #\] text :start i)))
                      (and at close (< at close)
                           (not (find #\Space text :start i :end close))
                           (every #'alpha-char-p (subseq text (1+ i) at)))))
               (let* ((at (position #\@ text :start i))
                      (close (position #\] text :start i))
                      (kind (subseq text (1+ i) at))
                      (target (subseq text (1+ at) close)))
                 (write-string (if (string= kind "id")
                                   (render-ref target (format nil "`~a`" target))
                                   (render-ref target (format nil "`~a`" target)))
                               out)
                 (setf i (1+ close))))
              ;; %TRUE %FALSE %NULL %GTK_ALIGN_CENTER
              ((and (char= c #\%) (< (1+ i) n) (identifier-char-p (char text (1+ i))))
               (let* ((end (scan-identifier text (1+ i)))
                      (word (subseq text (1+ i) end)))
                 (write-string (cond ((string= word "TRUE") "T")
                                     ((member word '("FALSE" "NULL") :test #'string=) "NIL")
                                     (t (render-ref word word)))
                               out)
                 (setf i end)))
              ;; #GtkWidget (an unknown #word, such as a URL fragment, stays as written)
              ((and (char= c #\#) (< (1+ i) n) (alpha-char-p (char text (1+ i))))
               (let* ((end (scan-identifier text (1+ i)))
                      (word (subseq text (1+ i) end)))
                 (write-string (render-ref word (subseq text i end)) out)
                 (setf i end)))
              ;; @param -> PARAM
              ((and (char= c #\@) (< (1+ i) n) (alpha-char-p (char text (1+ i))))
               (let ((end (scan-identifier text (1+ i))))
                 (write-string (string-upcase (snake-to-kebab (subseq text (1+ i) end))) out)
                 (setf i end)))
              ;; `GtkButton`, `gtk_widget_show()`, `GTK_ALIGN_CENTER`: a backquoted C name
              ((and (char= c #\`) (< (1+ i) n) (identifier-char-p (char text (1+ i)))
                    (let* ((end (scan-identifier text (1+ i)))
                           (end* (if (and (< (1+ end) n) (string= "()" (subseq text end (+ end 2))))
                                     (+ end 2) end)))
                      (and (< end* n) (char= (char text end*) #\`)
                           (gethash (subseq text (1+ i) end) *doc-index*))))
               (let* ((end (scan-identifier text (1+ i)))
                      (end* (if (and (< (1+ end) n) (string= "()" (subseq text end (+ end 2))))
                                (+ end 2) end)))
                 (write-string (render-ref (subseq text (1+ i) end) "") out)
                 (setf i (1+ end*))))
              ;; gtk_widget_show() and `gtk_widget_show()`
              ((and (identifier-char-p c)
                    (or (zerop i) (not (identifier-char-p (char text (1- i))))))
               (let* ((end (scan-identifier text i))
                      (word (subseq text i end))
                      (call (and (< (1+ end) n) (string= "()" (subseq text end (+ end 2)))))
                      (r (and (find #\_ word) (gethash word *doc-index*))))
                 (cond
                   ((and r call)
                    ;; Drop surrounding backquotes: render-ref adds its own.
                    (let ((quoted (and (> i 0) (char= (char text (1- i)) #\`)
                                       (< (+ end 2) n) (char= (char text (+ end 2)) #\`))))
                      (when quoted
                        (let ((s (get-output-stream-string out)))
                          (write-string (subseq s 0 (1- (length s))) out)))
                      (write-string (render-ref word word) out)
                      (setf i (+ end 2 (if quoted 1 0)))))
                   (t (write-string word out) (setf i end)))))
              (t (write-char c out) (incf i)))))))))

(defun first-paragraph (text)
  (when text
    (let ((end (search (format nil "~%~%") text)))
      (string-trim '(#\Space #\Newline) (subseq text 0 end)))))

;;; Describing Lisp values, for docstrings and the reference site

(defun describe-spec (spec nsname)
  "A short phrase for the Lisp value SPEC stands for."
  (declare (ignorable nsname))
  (flet ((ref (sym) (if sym (format nil "`~(~a:~a~)`" (package-name (symbol-package sym)) (symbol-name sym))
                        "a value")))
    (case (spec-kind* spec)
      (:boolean "a boolean")
      ((:int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :short :ushort
        :int :uint :long :ulong :size :ssize :intptr :uintptr) "an integer")
      ((:float :double) "a float")
      (:gtype "a GType")
      (:string "a string")
      (:strv "a list of strings")
      (:object (format nil "a ~a" (ref (second spec))))
      (:boxed (if (fourth spec) (format nil "a ~a" (ref (fourth spec))) "a boxed value"))
      (:record (format nil "a ~a" (ref (second spec))))
      ((:enum :flags) (format nil "~a ~a" (if (eq (spec-kind* spec) :flags) "a list of" "a")
                              (ref (second spec))))
      (:array (if (eq (second spec) :uint8) "an octet vector"
                  (format nil "a list of ~a" (strip-article (describe-spec (second spec) nsname)))))
      (:byte-array "an octet vector")
      ((:glist :gslist :gptrarray) (format nil "a list of ~a" (strip-article (describe-spec (second spec) nsname))))
      (:ghash "a hash table")
      (:gvalue "a Lisp value")
      (:callback "a function")
      (:pointer "a foreign pointer")
      (t "a value"))))

(defun strip-article (phrase)
  (cond ((and (> (length phrase) 2) (string= "a " (subseq phrase 0 2))) (subseq phrase 2))
        ((and (> (length phrase) 3) (string= "an " (subseq phrase 0 3))) (subseq phrase 3))
        (t phrase)))

(defun plan-lambda-list (plan)
  "The Lisp lambda list of PLAN's function, as a string."
  (let* ((visible (remove-if (lambda (a)
                               (let ((o (cddr a)))
                                 (or (eq (getf o :direction) :out) (getf o :user-data-of)
                                     (getf o :destroy-of) (getf o :length-of))))
                             (plan-args plan)))
         (required (remove-if (lambda (a) (getf (cddr a) :optional)) visible))
         (optional (remove-if-not (lambda (a) (getf (cddr a) :optional)) visible)))
    (format nil "~{~(~a~)~^ ~}~@[ &optional~{ ~(~a~)~}~]"
            (mapcar #'first required) (and optional (mapcar #'first optional)))))

(defun plan-values-description (plan nsname)
  "A sentence describing what PLAN's function returns, or NIL."
  (let ((items (append
                (unless (eq (plan-return plan) :void)
                  (list (describe-spec (plan-return plan) nsname)))
                (loop for a in (plan-args plan)
                      for o = (cddr a)
                      when (and (eq (getf o :direction) :out) (not (getf o :length-of)))
                        collect (format nil "~a (~a)" (string-upcase (symbol-name (first a)))
                                        (describe-spec (second a) nsname))))))
    (cond ((null items) nil)
          ((null (rest items)) (format nil "Returns ~a." (first items)))
          (t (format nil "Returns ~{~a~^, then ~} as multiple values." items)))))

(defun plan-docstring (plan nsname url)
  "The docstring for a generated function: summary, what it returns and
signals, then its C name, upstream page and version."
  (let ((notes (remove nil
                       (list* (plan-values-description plan nsname)
                              (when (plan-throws plan) "Signals `glib:glib-error` on failure.")
                              (loop for a in (plan-args plan)
                                    when (eq (spec-kind* (second a)) :callback)
                                      collect (format nil "~a is a function or a symbol naming one~a."
                                                      (string-upcase (symbol-name (first a)))
                                                      (case (third (second a))
                                                        (:call ", called before this function returns")
                                                        (:async ", called once when the operation completes")
                                                        (:notified ", kept until GTK no longer needs it")
                                                        (t ""))))))))
    (join-paragraphs
     (list (convert-doc-text (first-paragraph (plan-doc plan)))
           (and notes (format nil "~{~a~^~%~}" notes))
           (reference-lines (plan-c-name plan) url (plan-version plan) (plan-deprecated plan))))))

(defun reference-lines (c-name url version deprecated)
  (format nil "~@[C: ~a~]~@[~%See: ~a~]~@[~%Since: ~a~]~:[~;~%Deprecated.~]"
          c-name url version deprecated))

(defun join-paragraphs (parts)
  (format nil "~{~a~^~%~%~}" (remove-if (lambda (p) (or (null p) (string= p ""))) parts)))
