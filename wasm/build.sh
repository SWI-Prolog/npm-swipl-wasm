#!/bin/bash

set -o errexit
set -o xtrace

rm -rf wasm/swipl-devel
rm -rf wasm/zlib-1.2.12

./wasm/build-zlib.sh
./wasm/clone-swipl.sh
./wasm/build-swipl-native.sh
./wasm/build-swipl-wasm.sh
./wasm/extract-wasm.sh
