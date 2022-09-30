#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

# SWI-Prolog

mkdir -p /swipl-devel/build.wasm
cd /swipl-devel/build.wasm
cmake -DCMAKE_TOOLCHAIN_FILE=/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DZLIB_LIBRARY=/zlib-$ZLIB_VERSION/libz.a \
  -DZLIB_INCLUDE_DIR=/zlib-$ZLIB_VERSION \
  -DGMP_ROOT=/gmp \
  -DINSTALL_DOCUMENTATION=OFF \
  -G Ninja ..
ninja
