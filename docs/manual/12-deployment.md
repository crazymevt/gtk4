# Deployment

A gtk4 program ships as one executable file containing Lisp, the bindings and your code. GTK itself comes from the system, from the application bundle on macOS, or from the Flatpak runtime.

## Executables

Load your program in a fresh Lisp, without creating any GTK objects, and save:

```
sbcl --non-interactive --eval '(ql:quickload :my-app)' \
     --eval '(gtk4:save-executable "my-app" (lambda () (my-app:main)))'
```

`gtk4:save-executable` saves an executable that calls your function and exits with its value when that is an integer (an exit status), else 0. Before saving, the bindings forget everything that only made sense in the running process: C function addresses, GType ids, proxies. When the executable starts, they load the GTK libraries again and look everything up afresh, so Lisp-defined classes register their GTypes in the new process as usual.

Save from a script (`--non-interactive`), not from an editor's REPL: SBCL can only save an image with a single thread running. GTK objects created before saving do not survive into the executable; create them in your main function.

`make executable NAME=clock` saves `examples/clock.lisp` as `build/clock`.

## Where the libraries are found

At startup, the bindings look for the GTK libraries in this order:

1. The directories in `GTK4_LISP_LIBRARY_PATH`, separated by `:` (`;` on Windows).
2. `lib/` beside the executable.
3. `../Frameworks/` relative to the executable (a macOS application bundle).
4. On macOS, the Homebrew and MacPorts library directories.
5. The system's usual search path.

## macOS application bundles

`scripts/macos-app.sh` wraps an executable in an `.app`:

```
scripts/macos-app.sh build/clock Clock org.example.Clock               # uses the installed GTK
scripts/macos-app.sh build/clock Clock org.example.Clock --bundle-gtk  # carries its own GTK
```

With `--bundle-gtk`, the script:

- copies GTK and every library it uses (around 40, about 28 MB) into `Contents/Frameworks`;
- rewrites the libraries' references to each other to point there, and re-signs them;
- copies the GSettings schemas and icon themes GTK reads into `Contents/Resources`, where the program finds them when it starts.

Homebrew's SBCL links its runtime against `libzstd`, and an executable inherits that reference. The Lisp image appended to the executable stops tools from changing the reference afterwards. So save the executable with a runtime prepared by `scripts/macos-runtime.sh`, whose references already point into the bundle. `make app NAME=clock APP=Clock` does all of this.

The bundle is signed ad hoc, which is enough to run it on the Mac that built it. To distribute it, sign it with your Developer ID and notarize it.

## Flatpak

`packaging/flatpak/org.lisp.gtk4.Clock.yml` is a complete manifest for the clock example, to copy for your own program. The GNOME runtime provides GTK 4 and libadwaita. SBCL and the Lisp libraries are build-only sources, pinned with checksums because Flatpak builds have no network access, and only the saved executable ships. Two settings matter:

- `strip: false`: stripping a saved executable cuts off its Lisp image.
- `--dynamic-space-size 4096`: compiling all the bindings needs a larger heap than some SBCL builds default to.

```
flatpak-builder --user --install-deps-from=flathub --force-clean build-dir \
    packaging/flatpak/org.lisp.gtk4.Clock.yml
flatpak-builder --run build-dir packaging/flatpak/org.lisp.gtk4.Clock.yml clock
```

## Linux without Flatpak

An executable runs anywhere the GTK 4.14 (or newer) runtime libraries are installed: `libgtk-4-1` on Debian and Ubuntu, `gtk4` on Fedora and Arch. Ship it alone, or in a distribution package that depends on GTK.

## Windows

Windows is a tier-2 platform: the full test suite runs there in CI, but a failure does not block a release. Install GTK (and libadwaita) through [MSYS2](https://www.msys2.org/):

```
pacman -S mingw-w64-ucrt-x86_64-gtk4 mingw-w64-ucrt-x86_64-libadwaita
```

Then put MSYS2's `ucrt64\bin` directory in `GTK4_LISP_LIBRARY_PATH` or on `PATH`, and its `ucrt64\share` in `XDG_DATA_DIRS`. Executables saved with `gtk4:save-executable` need the same libraries; ship them in a `lib\` directory beside the executable.
