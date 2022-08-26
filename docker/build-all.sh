#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

# ZLIB

cd /zlib-1.2.12
emconfigure ./configure
EMCC_CFLAGS=-Wno-deprecated-non-prototype emmake make

# SWI-Prolog

mkdir /swipl-devel/swipl.wasm
cd /swipl-devel/swipl.wasm
cmake -DCMAKE_TOOLCHAIN_FILE=/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DZLIB_LIBRARY=/zlib-1.2.12/libz.a \
  -DZLIB_INCLUDE_DIR=/zlib-1.2.12 \
  -DINSTALL_DOCUMENTATION=OFF \
  -DMULTI_THREADED=OFF \
  -DUSE_SIGNALS=OFF \
  -DUSE_GMP=OFF \
  -DBUILD_SWIPL_LD=OFF \
  -DSWIPL_PACKAGES=OFF \
  -G Ninja ..
ninja
