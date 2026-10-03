SBCL ?= sbcl
LISP = $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'
QUIT_AFTER ?= nil

.PHONY: test stress summary generate docs full-stack hello example demo

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

docs:
	$(LISP) --eval '(ql:quickload :gtk4-generator :silent t)' \
	        --eval '(gtk4.generator:generate :site-directory (merge-pathnames "build/docs/" (truename ".")))' \
	        | tail -1

full-stack:
	$(SBCL) --non-interactive --load scripts/full-stack.lisp compile | grep 'of all namespaces'
	$(SBCL) --non-interactive --load scripts/full-stack.lisp load | grep 'of all namespaces'

hello:
	$(LISP) --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/hello-world.lisp \
	        --eval '(gtk4-examples.hello-world:main :quit-after $(QUIT_AFTER))'

# Run any example: make example NAME=drawing [QUIT_AFTER=3]
example:
	$(LISP) --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/$(NAME).lisp \
	        --eval '(gtk4-examples.$(NAME):main :quit-after $(QUIT_AFTER))'

# The demo browser: make demo [QUIT_AFTER=3]
demo:
	$(LISP) --eval '(ql:quickload :gtk4-demo :silent t)' \
	        --eval '(gtk4-demo:main :quit-after $(QUIT_AFTER))'
