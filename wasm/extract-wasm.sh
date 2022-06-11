#!/bin/bash

# Extracts distributed files from the build directory.

set -o errexit
set -o xtrace

mkdir -p wasm/dist
cp wasm/swipl-devel/build.wasm/src/swipl-web.data wasm/dist/swipl-web.data
cp wasm/swipl-devel/build.wasm/src/swipl-web.wasm wasm/dist/swipl-web.wasm
cp wasm/swipl-devel/build.wasm/src/swipl-web.js wasm/dist/swipl-web.js
