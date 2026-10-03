SBCL ?= sbcl
LISP = $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'
QUIT_AFTER ?= nil

.PHONY: test stress summary generate full-stack hello

test:
	$(LISP) --eval '(ql:quickload :gtk4-tests :silent t)' \
	        --eval '(uiop:quit (if (parachute:status (parachute:test :gtk4-tests)) 0 1))'

stress:
	$(LISP) --eval '(ql:quickload :gtk4-tests/stress :silent t)' \
	        --eval '(uiop:quit (if (parachute:status (parachute:test (quote gtk4-tests::gtk4-stress))) 0 1))'

summary:
	$(LISP) --eval '(ql:quickload :gtk4-generator :silent t)' \
	        --eval '(gtk4.generator:print-summary (gtk4.generator:load-targets))'

generate:
	$(LISP) --eval '(ql:quickload :gtk4-generator :silent t)' \
	        --eval '(gtk4.generator:generate)'

full-stack:
	$(SBCL) --non-interactive --load scripts/full-stack.lisp generate | sed -n '/^| Namespace/,/^| \*\*Total/p'
	$(SBCL) --non-interactive --load scripts/full-stack.lisp compile | grep 'of all namespaces'
	$(SBCL) --non-interactive --load scripts/full-stack.lisp load | grep 'of all namespaces'

hello:
	$(LISP) --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/hello-world.lisp \
	        --eval '(gtk4-examples.hello-world:main :quit-after $(QUIT_AFTER))'
