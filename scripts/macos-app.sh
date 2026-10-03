#!/bin/bash
# Make a macOS application bundle from an executable saved with
# gtk4:save-executable.
#
#   scripts/macos-app.sh build/clock Clock org.example.Clock [--bundle-gtk]
#
# Writes build/Clock.app. Without --bundle-gtk the app uses the GTK installed
# on the Mac running it (Homebrew's, for example). With --bundle-gtk it
# carries its own copy: GTK and every non-system library it needs go into
# Contents/Frameworks, with their references rewritten to point there, and
# the GSettings schemas and icon themes GTK reads go into Contents/Resources.
set -euo pipefail

if [ $# -lt 3 ]; then
  echo "usage: $0 EXECUTABLE NAME BUNDLE-ID [--bundle-gtk]" >&2
  exit 2
fi
exe=$1 name=$2 id=$3 bundle_gtk=${4:-}
prefix=${HOMEBREW_PREFIX:-$(brew --prefix 2>/dev/null || echo /opt/homebrew)}
app="$(dirname "$exe")/$name.app"

rm -rf "$app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources"
cp "$exe" "$app/Contents/MacOS/$name"

cat > "$app/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key><string>$name</string>
  <key>CFBundleIdentifier</key><string>$id</string>
  <key>CFBundleName</key><string>$name</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
  <key>LSMinimumSystemVersion</key><string>12.0</string>
  <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST

if [ "$bundle_gtk" = "--bundle-gtk" ]; then
  frameworks="$app/Contents/Frameworks"
  mkdir -p "$frameworks"
  # The libraries the bindings load by name (see src/runtime/libraries.lisp).
  queue=()
  for lib in libglib-2.0.0 libgobject-2.0.0 libgmodule-2.0.0 libgio-2.0.0 libcairo.2 \
             libcairo-gobject.2 libharfbuzz.0 libharfbuzz-gobject.0 libpango-1.0.0 \
             libpangocairo-1.0.0 libgraphene-1.0.0 libgdk_pixbuf-2.0.0 libgtk-4.1 libadwaita-1.0; do
    [ -e "$prefix/lib/$lib.dylib" ] && queue+=("$prefix/lib/$lib.dylib")
  done
  # Copy each library and, transitively, every non-system library it uses.
  while [ ${#queue[@]} -gt 0 ]; do
    src=${queue[0]}; queue=("${queue[@]:1}")
    real=$(realpath "$src"); base=$(basename "$src")
    [ -e "$frameworks/$base" ] && continue
    cp "$real" "$frameworks/$base"
    chmod u+w "$frameworks/$base"
    while read -r dep; do
      case "$dep" in
        /usr/lib/*|/System/*|@*) ;;
        *) queue+=("$dep") ;;
      esac
    done < <(otool -L "$real" | tail -n +2 | awk '{print $1}')
  done
  # Point every reference at the copies beside it, then re-sign (arm64 Macs
  # refuse to load a modified library whose signature no longer matches).
  for lib in "$frameworks"/*.dylib; do
    install_name_tool -id "@loader_path/$(basename "$lib")" "$lib" 2>/dev/null
    otool -L "$lib" | tail -n +2 | awk '{print $1}' | while read -r dep; do
      case "$dep" in
        /usr/lib/*|/System/*|@*) ;;
        *) install_name_tool -change "$dep" "@loader_path/$(basename "$dep")" "$lib" 2>/dev/null ;;
      esac
    done
    codesign --force --sign - "$lib" 2>/dev/null
  done
  # Data GTK reads at run time; the bindings point XDG_DATA_DIRS here.
  share="$app/Contents/Resources/share"
  mkdir -p "$share/glib-2.0/schemas" "$share/icons"
  cp "$prefix/share/glib-2.0/schemas/"*.xml "$share/glib-2.0/schemas/" 2>/dev/null || true
  glib-compile-schemas "$share/glib-2.0/schemas"
  for theme in Adwaita hicolor; do
    [ -d "$prefix/share/icons/$theme" ] && cp -R "$prefix/share/icons/$theme" "$share/icons/"
  done
  # The executable's own references (the SBCL runtime's) must already point
  # into the bundle: see scripts/macos-runtime.sh.
  otool -L "$app/Contents/MacOS/$name" | tail -n +2 | awk '{print $1}' | while read -r dep; do
    case "$dep" in
      /usr/lib/*|/System/*) ;;
      @executable_path/../Frameworks/*) cp -n "$(dirname "$exe")/app-runtime/Frameworks/$(basename "$dep")" "$frameworks/" 2>/dev/null || true ;;
      *) echo "warning: the executable needs $dep, which is not bundled; save it with the runtime from scripts/macos-runtime.sh" >&2 ;;
    esac
  done
  echo "Bundled $(ls "$frameworks" | wc -l | tr -d ' ') libraries into $frameworks"
fi

echo "Wrote $app"
