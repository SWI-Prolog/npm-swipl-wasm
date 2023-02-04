# Builds SWI-Prolog WebAssembly version.
# https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650

FROM debian:bullseye

# Installs build dependencies.

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y \
  bzip2 \
  cmake \
  git \
  lzip \
  ninja-build \
  python3 \
  wget \
  xz-utils

RUN git config --global pull.ff only

# Installs Emscripten.
# See https://emscripten.org/docs/getting_started/downloads.html

ENV EMSDK_VERSION 3.1.21

RUN git clone https://github.com/emscripten-core/emsdk.git /emsdk
WORKDIR /emsdk
RUN git pull
RUN ./emsdk install $EMSDK_VERSION
RUN ./emsdk activate $EMSDK_VERSION

# Downloads and builds ZLIB.

ENV ZLIB_VERSION 1.2.13

RUN wget --no-verbose https://zlib.net/zlib-$ZLIB_VERSION.tar.gz -O /zlib-$ZLIB_VERSION.tar.gz
RUN tar -xf /zlib-$ZLIB_VERSION.tar.gz --directory /
COPY build-zlib.sh /build-zlib.sh
RUN chmod +x /build-zlib.sh
RUN /build-zlib.sh

# Downloads and builds GMP.

ENV GMP_VERSION 6.2.1

RUN wget --no-verbose https://gmplib.org/download/gmp/gmp-$GMP_VERSION.tar.lz -O /gmp-$GMP_VERSION.tar.lz
RUN tar -xf /gmp-$GMP_VERSION.tar.lz --directory /
COPY build-gmp.sh /build-gmp.sh
RUN chmod +x /build-gmp.sh
RUN /build-gmp.sh

# Clones SWI-Prolog.

RUN git clone https://github.com/SWI-Prolog/swipl-devel.git /swipl-devel
RUN git submodule update --init

# Checkout a specific commit
ENV SWIPL_COMMIT d08ed2415e67b5b9e7df16a64d88041aeb0dd654

WORKDIR /swipl-devel
RUN git pull && git submodule update --init && \
    git checkout $SWIPL_COMMIT -b local-wasm

# Compiles everything.

COPY build-swipl.sh /build-swipl.sh
RUN chmod +x /build-swipl.sh
RUN /build-swipl.sh
