#!/bin/bash

# Clones SWI-Prolog repository and its submodules.

set -o errexit
set -o xtrace

cd wasm
git clone https://github.com/SWI-Prolog/swipl-devel.git
cd swipl-devel
git checkout tags/V8.5.12 -b local-wasm
git fetch
git submodule update --init
