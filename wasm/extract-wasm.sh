#!/bin/bash

# Extracts distributed files from the build directory.

set -o errexit
set -o xtrace

mkdir -p src/swipl
cp wasm/swipl-devel/build.wasm/src/swipl-web.data src/swipl/swipl-web.data
cp wasm/swipl-devel/build.wasm/src/swipl-web.wasm src/swipl/swipl-web.wasm
cp wasm/swipl-devel/build.wasm/src/swipl-web.js src/swipl/swipl-web.js
