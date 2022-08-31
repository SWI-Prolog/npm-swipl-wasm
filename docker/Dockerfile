# Builds SWI-Prolog WebAssembly version.
# https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650

FROM debian:bullseye

# Installs build dependencies.

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y \
  bzip2 \
  cmake \
  git \
  ninja-build \
  python3 \
  wget \
  xz-utils

RUN git config --global pull.ff only

# Installs Emscripten.
# See https://emscripten.org/docs/getting_started/downloads.html

ENV EMSDK_VERSION 3.1.20

RUN git clone https://github.com/emscripten-core/emsdk.git /emsdk
WORKDIR /emsdk
RUN git pull
RUN ./emsdk install $EMSDK_VERSION
RUN ./emsdk activate $EMSDK_VERSION

# Downloads ZLIB.

ENV ZLIB_VERSION 1.2.12

RUN wget --no-verbose https://zlib.net/zlib-$ZLIB_VERSION.tar.gz -O /zlib-$ZLIB_VERSION.tar.gz
RUN tar -xf /zlib-$ZLIB_VERSION.tar.gz --directory /

# Clones SWI-Prolog.

RUN git clone https://github.com/SWI-Prolog/swipl-devel.git /swipl-devel
WORKDIR /swipl-devel

# Uncomment the following to build a specific commit.
# ENV SWIPL_COMMIT 5c7d1f8352b27d45cb8066eac5af95cf925a05b7
# RUN git checkout $SWIPL_COMMIT -b local-wasm

RUN git fetch
RUN git submodule update --init

# Compiles everything.

COPY build-all.sh /build-all.sh
RUN chmod +x /build-all.sh
RUN /build-all.sh
