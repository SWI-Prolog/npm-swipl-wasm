# Builds SWI-Prolog WebAssembly version.
# https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
ARG EMSDK_VERSION=latest
FROM emscripten/emsdk:$EMSDK_VERSION

# Installs build dependencies.
RUN apt-get update && apt-get install -y ninja-build rsync

# Download dependency sources
WORKDIR /wasm
ARG ZLIB_VERSION
ARG PCRE2_NAME
RUN wget --no-verbose -qO- https://zlib.net/zlib-$ZLIB_VERSION.tar.gz | tar xvz
RUN git clone --branch=$PCRE2_NAME --depth 1 https://github.com/PCRE2Project/pcre2

# Build the dependencies and install them in /wasm
WORKDIR /wasm/zlib-$ZLIB_VERSION
RUN emconfigure ./configure --static --prefix=/wasm
RUN EMCC_CFLAGS=-Wno-deprecated-non-prototype emmake make
RUN emmake make install

WORKDIR /wasm/pcre2/build
RUN emcmake cmake -DCMAKE_INSTALL_PREFIX=/wasm \
  -DPCRE2GREP_SUPPORT_JIT=OFF \
  -G Ninja .. $@
RUN ninja && ninja install

# Clone SWI-Prolog.  Only make a shallow clone and only clone the
# submodules we need at depth 1.
WORKDIR /
ARG SWIPL_COMMIT
RUN git clone --single-branch https://github.com/SWI-Prolog/swipl-devel

WORKDIR /swipl-devel
RUN git checkout $SWIPL_COMMIT
RUN git submodule update --init --depth 1 -j 100 \
    packages/chr packages/clib packages/clpqr packages/http packages/json \
    packages/nlp packages/pcre packages/plunit packages/sgml packages/RDF \
    packages/semweb packages/zlib

# Add the swipl-web-no-data link target: an external (streamable) .wasm
# with no .data preload and no base64 embedding.  The snippet is
# appended (rather than applied as a context patch) so it keeps working
# across SWIPL_COMMIT bumps, and it is guarded to become a no-op if
# swipl-devel ever ships the target itself.
COPY swipl-web-no-data.cmake /tmp/swipl-web-no-data.cmake
RUN cat /tmp/swipl-web-no-data.cmake >> /swipl-devel/cmake/EmscriptenTargets.cmake

# Build SWIPL
WORKDIR /swipl-devel/build.wasm
RUN mkdir -p /swipl-devel/build.wasm
ENV LDFLAGS="-s STACK_SIZE=1048576"
RUN cmake -DCMAKE_TOOLCHAIN_FILE=/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_FIND_ROOT_PATH=/wasm \
  -DUSE_GMP=OFF \
  -DINSTALL_DOCUMENTATION=OFF \
  -DINSTALL_QLF=ON \
  -DINSTALL_PROLOG_SRC=OFF \
  -G Ninja ..
RUN ninja

RUN ctest -j $(nproc) --stop-on-failure --output-on-failure
