;;;; declarative.lisp — building a window with gtk:build and gtk:css
;;;; GTK docs: https://docs.gtk.org/gtk4/class.Grid.html

(in-package #:gtk4-demo)

(defvar *declarative-css* nil)

(defun declarative-greet (button)
  (gtk:button-set-label button "Greeted!"))

(define-demo declarative
    (:title "Declarative UI"
     :category "Basics"
     :description "The whole window is one gtk:build form: widgets, properties, grid
positions (:layout), a header bar child (:child-type), signal handlers (:on-clicked) and
ids for later lookup. Its style comes from gtk:add-css rules written as s-expressions.")
  (unless *declarative-css*
    (setf *declarative-css*
          (gtk:add-css '((".declarative-title" :font-size "16pt" :font-weight :bold)
                         (".declarative-card" :padding "12px" :border-radius "12px"
                          :background "alpha(currentColor, 0.06)")))))
  (multiple-value-bind (window ids)
      (gtk:build
        (gtk:window :title "Declarative UI" :default-width 380 :default-height 300
          (gtk:header-bar :child-type "titlebar"
            (gtk:button :id :reset :label "Reset" :child-type "start"))
          (gtk:box :orientation :vertical :spacing 12 :margin-top 18 :margin-bottom 18
                   :margin-start 18 :margin-end 18
            (gtk:label :label "Your details" :xalign 0.0 :css-classes '("declarative-title"))
            (gtk:grid :column-spacing 12 :row-spacing 8 :css-classes '("declarative-card")
              (gtk:label :label "Name" :xalign 1.0 :layout (:column 0 :row 0))
              (gtk:entry :id :name :hexpand t :layout (:column 1 :row 0))
              (gtk:label :label "Volume" :xalign 1.0 :layout (:column 0 :row 1))
              (gtk:scale :id :volume :hexpand t :layout (:column 1 :row 1)
                         :adjustment (gtk:adjustment-new 50d0 0d0 100d0 1d0 10d0 0d0))
              (gtk:check-button :id :notify :label "Send notifications" :layout (:column 1 :row 2)))
            (gtk:button :id :greet :label "Greet" :halign :end :css-classes '("suggested-action")
                        :on-clicked 'declarative-greet))))
    (gobject:connect (gethash :reset ids) :clicked
                     (lambda (b) (declare (ignore b))
                       (gtk:editable-set-text (gethash :name ids) "")
                       (gtk:range-set-value (gethash :volume ids) 50d0)
                       (gtk:check-button-set-active (gethash :notify ids) nil)
                       (gtk:button-set-label (gethash :greet ids) "Greet")))
    window))
