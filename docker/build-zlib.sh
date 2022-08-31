#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

cd /zlib-$ZLIB_VERSION
emconfigure ./configure
EMCC_CFLAGS=-Wno-deprecated-non-prototype emmake make
