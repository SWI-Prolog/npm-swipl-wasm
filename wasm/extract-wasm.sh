#!/bin/bash

# Extracts distributed files from the build directory.

set -o errexit
set -o xtrace

mkdir -p dist/swipl
cp wasm/swipl-devel/build.wasm/src/swipl-web.data dist/swipl/swipl-web.data
cp wasm/swipl-devel/build.wasm/src/swipl-web.wasm dist/swipl/swipl-web.wasm
cp wasm/swipl-devel/build.wasm/src/swipl-web.js dist/swipl/swipl-web.js
