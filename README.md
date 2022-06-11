# swi-wasm

SWI-Prolog WebAssembly build as a NPM package.

## Build

It is recommended to use yarn for building. It was used for development
of this package.

## Development

You need to install the following software for local development:

- emscripten: <https://emscripten.org/docs/getting_started/downloads.html>

Linux users need Python 3, cmake, ninja-build and zlib development files.
The local development scripts assume that emscripten is installed to ~/emsdk
as in the official instructions.

It is recommended to use VS Code IDE as it was used for development of
this package.

## Hacking SWI source code and build system

Please see cmake files:

- swipl-devel/cmake/EmscriptenTargets.cmake
- swipl-devel/cmake/port/Emscripten.cmake

## TODO

- Turn SWI-Prolog version tag into a variable (in clone-swipl.sh).
- Fix emscripten version into place.
- Distribute zlib?
- Use EcmaScript modules as output:
  <https://github.com/emscripten-core/emscripten/issues/11792>
- Zlib location is hard-coded
