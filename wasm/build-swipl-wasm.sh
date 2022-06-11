#!/bin/bash

# Builds SWI-Prolog WebAssembly version.

set -o errexit
set -o xtrace

cd wasm/swipl-devel

# Builds WebAssembly version. This build uses the previously built
# native version to generate the boot files.

source ~/emsdk/emsdk_env.sh
mkdir -p build.wasm
cd build.wasm
cmake -DCMAKE_TOOLCHAIN_FILE=~/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
      -DCMAKE_BUILD_TYPE=Release \
      -DZLIB_LIBRARY=/home/raivo/swi-wasm/wasm/zlib-1.2.12/libz.a \
      -DZLIB_INCLUDE_DIR=/home/raivo/swi-wasm/wasm/zlib-1.2.12 \
      -DMULTI_THREADED=OFF \
      -DUSE_SIGNALS=OFF \
      -DUSE_GMP=OFF \
      -DBUILD_SWIPL_LD=OFF \
      -DSWIPL_PACKAGES=OFF \
      -DINSTALL_DOCUMENTATION=OFF \
      -DSWIPL_NATIVE_FRIEND=build \
      -G Ninja ..
ninja
