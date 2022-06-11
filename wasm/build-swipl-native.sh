#!/bin/bash

# Builds SWI-Prolog native version.

set -o errexit
set -o xtrace

cd wasm/swipl-devel

# Builds native version. The native version is used for generating
# so-called boot files. We are currently unable to generate them
# with the WebAssembly version solely.

mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Release \
      -DMULTI_THREADED=OFF \
      -DUSE_SIGNALS=OFF \
      -DUSE_GMP=OFF \
      -DBUILD_SWIPL_LD=OFF \
      -DSWIPL_PACKAGES=OFF \
      -DINSTALL_DOCUMENTATION=OFF \
      -G Ninja ..
ninja
