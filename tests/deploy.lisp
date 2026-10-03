;;;; deploy.lisp — a saved executable starts and runs GTK in a new process

(in-package #:gtk4-tests)

(define-test deploy :parent gtk4-tests)

(define-test saved-executable-runs :parent deploy
  (with-gtk
    (let* ((dir (uiop:ensure-directory-pathname
                 (uiop:merge-pathnames* (format nil "gtk4-test-~d/" (random 1000000))
                                        (uiop:temporary-directory))))
           (exe (merge-pathnames "clock" dir))
           (root (namestring (asdf:system-relative-pathname "gtk4" ""))))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             ;; Save in a child Lisp, as a build script would.
             (uiop:run-program
              (list (or (uiop:getenv "SBCL") "sbcl") "--non-interactive"
                    "--eval" (format nil "(push #p~s asdf:*central-registry*)" root)
                    "--eval" "(ql:quickload :gtk4 :silent t)"
                    "--load" (namestring (asdf:system-relative-pathname "gtk4" "examples/clock.lisp"))
                    "--eval" (format nil "(gtk4:save-executable ~s (lambda () (gtk4-examples.clock:main :quit-after 1)))"
                                     (namestring exe)))
              :output nil :error-output nil)
             (true (probe-file exe))
             ;; The executable registers its Lisp GTypes and template anew.
             (multiple-value-bind (out err status)
                 (uiop:run-program (list (namestring exe)) :output :string :error-output :string
                                                           :ignore-error-status t)
               (declare (ignore out))
               (is = 0 status)
               (false (search "error" (string-downcase err)) "stderr: ~a" err)))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
