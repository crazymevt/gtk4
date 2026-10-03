SBCL ?= sbcl
LISP = $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'
QUIT_AFTER ?= nil

.PHONY: test summary hello

test:
	$(LISP) --eval '(ql:quickload :gtk4-tests :silent t)' \
	        --eval '(uiop:quit (if (parachute:status (parachute:test :gtk4-tests)) 0 1))'

summary:
	$(LISP) --eval '(ql:quickload :gtk4-generator :silent t)' \
	        --eval '(gtk4.generator:print-summary (gtk4.generator:load-targets))'

hello:
	$(LISP) --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/hello-world.lisp \
	        --eval '(gtk4-examples.hello-world:main :quit-after $(QUIT_AFTER))'
