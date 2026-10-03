#!/bin/bash
# Prepare an SBCL runtime for application bundles.
#
#   scripts/macos-runtime.sh build/app-runtime
#
# Some SBCL builds (Homebrew's, for one) link the runtime against libzstd
# for core compression. An executable saved by that runtime inherits the
# absolute path to it, and the saved image appended to the executable stops
# install_name_tool from changing it afterwards. So change it before saving:
# copy the runtime to DIR/MacOS/sbcl, its non-system libraries to
# DIR/Frameworks, and point the runtime at them the way an app bundle lays
# them out (Contents/MacOS and Contents/Frameworks). Executables saved by
# DIR/MacOS/sbcl then load those libraries from their own bundle.
set -euo pipefail
dir=${1:?usage: $0 DIR}
sbcl=${SBCL:-sbcl}
runtime=$("$sbcl" --noinform --non-interactive --no-userinit --eval '(princ sb-ext:*runtime-pathname*)')
core=$("$sbcl" --noinform --non-interactive --no-userinit --eval '(princ sb-ext:*core-pathname*)')

rm -rf "$dir"
mkdir -p "$dir/MacOS" "$dir/Frameworks"
cp "$runtime" "$dir/MacOS/sbcl"
chmod u+w "$dir/MacOS/sbcl"
otool -L "$runtime" | tail -n +2 | awk '{print $1}' | while read -r dep; do
  case "$dep" in
    /usr/lib/*|/System/*|@*) ;;
    *) cp "$(realpath "$dep")" "$dir/Frameworks/$(basename "$dep")"
       install_name_tool -change "$dep" "@executable_path/../Frameworks/$(basename "$dep")" "$dir/MacOS/sbcl" ;;
  esac
done
codesign --force --sign - "$dir/MacOS/sbcl" 2>/dev/null
# Run with: DIR/MacOS/sbcl --core CORE (the core is unchanged).
echo "$core" > "$dir/core-path"
echo "Runtime ready in $dir (core: $core)"
