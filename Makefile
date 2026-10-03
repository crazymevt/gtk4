SBCL ?= sbcl
# Compiling all the bindings needs more than the 1 GB heap some SBCL builds default to.
HEAP ?= 4096
LISP = $(SBCL) --dynamic-space-size $(HEAP) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'
QUIT_AFTER ?= nil

.PHONY: test stress summary generate docs full-stack hello example demo executable app

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
	$(LISP) --eval '(ql:quickload (list :gtk4 :gtk4-adwaita) :silent t)' \
	        --load examples/$(NAME).lisp \
	        --eval '(gtk4-examples.$(NAME):main :quit-after $(QUIT_AFTER))'

# The demo browser: make demo [QUIT_AFTER=3]
demo:
	$(LISP) --eval '(ql:quickload :gtk4-demo :silent t)' \
	        --eval '(gtk4-demo:main :quit-after $(QUIT_AFTER))'

# A standalone executable of an example: make executable NAME=clock, then ./build/clock
executable:
	mkdir -p build
	$(LISP) --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/$(NAME).lisp \
	        --eval '(gtk4:save-executable "build/$(NAME)" (lambda () (gtk4-examples.$(NAME):main :quit-after $(QUIT_AFTER))))'

# A macOS application bundle carrying its own GTK: make app NAME=clock APP=Clock
APP ?= $(NAME)
app:
	scripts/macos-runtime.sh build/app-runtime
	mkdir -p build
	SBCL_HOME="$$(dirname "$$(cat build/app-runtime/core-path)")" \
	build/app-runtime/MacOS/sbcl --core "$$(cat build/app-runtime/core-path)" --non-interactive \
	        --eval '(push (truename ".") asdf:*central-registry*)' \
	        --eval '(ql:quickload :gtk4 :silent t)' \
	        --load examples/$(NAME).lisp \
	        --eval '(gtk4:save-executable "build/$(NAME)" (lambda () (gtk4-examples.$(NAME):main :quit-after $(QUIT_AFTER))))'
	scripts/macos-app.sh build/$(NAME) $(APP) org.lisp.gtk4.$(APP) --bundle-gtk
