#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

# ZLIB

cd /zlib-$ZLIB_VERSION
emconfigure ./configure
EMCC_CFLAGS=-Wno-deprecated-non-prototype emmake make

# GMP

cd /gmp-$GMP_VERSION
mkdir /gmp
emconfigure ./configure --host=none --disable-assembly --prefix=/gmp
make
make install

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
  -DBUILD_SWIPL_LD=OFF \
  -DSWIPL_PACKAGES=OFF \
  -G Ninja ..
ninja
