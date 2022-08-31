#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

# SWI-Prolog

mkdir /swipl-devel/swipl.wasm
cd /swipl-devel/swipl.wasm
cmake -DCMAKE_TOOLCHAIN_FILE=/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DZLIB_LIBRARY=/zlib-$ZLIB_VERSION/libz.a \
  -DZLIB_INCLUDE_DIR=/zlib-$ZLIB_VERSION \
  -DINSTALL_DOCUMENTATION=OFF \
  -DMULTI_THREADED=OFF \
  -DUSE_SIGNALS=OFF \
  -DGMP_ROOT=/gmp \
  -DSTATIC_EXTENSIONS=ON \
  -DBUILD_SWIPL_LD=OFF \
  -G Ninja ..
ninja
