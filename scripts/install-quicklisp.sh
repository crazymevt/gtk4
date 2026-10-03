#!/bin/sh
# Install Quicklisp into ~/quicklisp (unless already there, e.g. restored from
# a CI cache) and have SBCL load it at startup.
set -eu
SBCL=${SBCL:-sbcl}
if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
  curl -fsSL -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
  "$SBCL" --non-interactive --load /tmp/quicklisp.lisp \
          --eval '(quicklisp-quickstart:install)'
fi
grep -qs quicklisp "$HOME/.sbclrc" || cat >> "$HOME/.sbclrc" <<'RC'
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))
RC
# Fetch the dependencies now, so a network failure shows up here rather than
# in the middle of the tests.
"$SBCL" --non-interactive \
        --eval '(ql:quickload (list :cffi :alexandria :plump :parachute) :silent t)'
