#!/bin/bash

# Downloads and builds zlib which is dependency of SWI-Prolog.

set -o errexit
set -o xtrace

source ~/emsdk/emsdk_env.sh

wget https://zlib.net/zlib-1.2.12.tar.gz -O wasm/zlib-1.2.12.tar.gz
cd wasm
tar -xf zlib-1.2.12.tar.gz
cd zlib-1.2.12
emconfigure ./configure
emmake make
