#!/bin/bash

set -o errexit
set -o xtrace

source /emsdk/emsdk_env.sh

cd /gmp-$GMP_VERSION
mkdir /gmp
emconfigure ./configure --host=none --disable-assembly --prefix=/gmp
make
make install
