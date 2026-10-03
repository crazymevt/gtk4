;;;; todo.lisp — a small application written with the Lisp layer
;;;;
;;;; A to-do list: the window is one gtk:build form, the tasks are Lisp
;;;; structures in a gio:list-store, the list view comes from
;;;; gtk:make-list-view, and the style from gtk:add-css.
;;;;
;;;; GTK documentation:
;;;;   https://docs.gtk.org/gtk4/class.ListView.html
;;;;   https://docs.gtk.org/gio/class.ListStore.html
;;;;
;;;; Run with:  make example NAME=todo   (QUIT_AFTER=3 to close automatically)

(defpackage #:gtk4-examples.todo
  (:use #:cl)
  (:export #:main))

(in-package #:gtk4-examples.todo)

(defstruct task title (done nil))

(defvar *css*
  '((".todo-done" :text-decoration :line-through :opacity "0.55")
    (".todo-count" :font-size "smaller")))

(defclass task-row (gtk:box)
  ((task :initform nil :accessor row-task)
   (check :reader row-check)
   (label :reader row-label)
   (on-change :initarg :on-change :initform nil :reader row-on-change))
  (:metaclass gobject:gobject-class)
  (:gtype-name "LispTodoRow")
  (:documentation "A row of the list: a check button and a label. List views
reuse rows for different tasks, so the row remembers which task it shows."))

(defmethod initialize-instance :after ((row task-row) &key)
  (let ((check (gtk:check-button-new))
        (label (make-instance 'gtk:label :xalign 0.0 :hexpand t)))
    (setf (slot-value row 'check) check
          (slot-value row 'label) label)
    (gtk:box-set-spacing row 8)
    (gtk:widget-set-margin-top row 4)
    (gtk:widget-set-margin-bottom row 4)
    (gtk:widget-set-margin-start row 8)
    (gtk:widget-set-margin-end row 8)
    (gtk:box-append row check)
    (gtk:box-append row label)
    ;; By symbol, so the handler does not capture the row.
    (gobject:connect check :toggled 'row-toggled)))

(defun show-done (row)
  (if (task-done (row-task row))
      (gtk:widget-add-css-class (row-label row) "todo-done")
      (gtk:widget-remove-css-class (row-label row) "todo-done")))

(defun row-toggled (check)
  (let ((row (gtk:widget-get-parent check)))
    (when (row-task row)
      (setf (task-done (row-task row)) (gtk:check-button-get-active check))
      (show-done row)
      (when (row-on-change row) (funcall (row-on-change row))))))

(defun bind-task (row task)
  (setf (row-task row) task)
  (gtk:label-set-text (row-label row) (task-title task))
  (gtk:check-button-set-active (row-check row) (task-done task))
  (show-done row))

(defun count-text (store)
  (let ((tasks (gio:list-model-items store)))
    (format nil "~d of ~d done" (count-if #'task-done tasks) (length tasks))))

(defun activate (app)
  (gtk:add-css *css*)
  (let* ((store (gio:make-list-store :items (list (make-task :title "Write the manual")
                                                  (make-task :title "Port the demos" :done t)
                                                  (make-task :title "Release 1.0"))))
         (count-label nil)
         (refresh (lambda ()
                    (when count-label (gtk:label-set-text count-label (count-text store))))))
    (multiple-value-bind (window ids)
        (gtk:build
          (gtk:application-window :application app :title "To do"
                                  :default-width 360 :default-height 420
            (gtk:box :orientation :vertical :spacing 8 :margin-top 12 :margin-bottom 12
                     :margin-start 12 :margin-end 12
              (gtk:box :spacing 6
                (gtk:entry :id :entry :hexpand t :placeholder-text "New task")
                (gtk:button :id :add :label "Add" :css-classes '("suggested-action")))
              (gtk:scrolled-window :vexpand t :has-frame t
                (gtk:make-list-view store
                                    :setup (lambda () (make-instance 'task-row :on-change refresh))
                                    :bind #'bind-task
                                    :selection :none))
              (gtk:label :id :count :xalign 0.0 :css-classes '("todo-count" "dim-label")))))
      (setf count-label (gethash :count ids))
      (flet ((add-task (&rest ignore)
               (declare (ignore ignore))
               (let ((title (string-trim " " (gtk:editable-get-text (gethash :entry ids)))))
                 (when (plusp (length title))
                   (gio:list-store-append store (gobject:make-lisp-object (make-task :title title)))
                   (gtk:editable-set-text (gethash :entry ids) "")))))
        (gobject:connect (gethash :add ids) :clicked #'add-task)
        (gobject:connect (gethash :entry ids) :activate #'add-task)
        (gobject:connect store :items-changed (lambda (&rest ignore)
                                                (declare (ignore ignore))
                                                (funcall refresh)))
        (funcall refresh))
      (gtk:window-present window))))

(defun main (&key quit-after)
  (let ((app (gtk:application-new "org.lisp.gtk4.Todo" '(:default-flags))))
    (gobject:connect app :activate #'activate)
    (when quit-after
      (glib:timeout-add-seconds glib:+priority-default+ quit-after
                                (lambda () (gio:application-quit app) nil)))
    (gio:application-run app nil)))
