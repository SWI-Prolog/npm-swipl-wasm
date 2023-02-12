# Builds SWI-Prolog WebAssembly version.
# https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
ARG EMSDK_VERSION
FROM emscripten/emsdk:$EMSDK_VERSION

# Installs build dependencies.
RUN apt-get update && apt-get install -y bzip2 lzip ninja-build xz-utils

# Downloads and builds ZLIB.
ARG ZLIB_VERSION
RUN wget --no-verbose -qO- https://zlib.net/zlib-$ZLIB_VERSION.tar.gz | tar xvz --directory /

WORKDIR /zlib-$ZLIB_VERSION
RUN emconfigure ./configure
RUN EMCC_CFLAGS=-Wno-deprecated-non-prototype emmake make

# Downloads and builds GMP.
ARG GMP_VERSION
RUN wget --no-verbose -qO- https://gmplib.org/download/gmp/gmp-$GMP_VERSION.tar.lz | tar x --lzip --directory /

WORKDIR /gmp-$GMP_VERSION
RUN mkdir /gmp
RUN emconfigure ./configure --host=none --disable-assembly --prefix=/gmp
RUN make
RUN make install

# Clones SWI-Prolog.
ARG SWIPL_VERSION
RUN git clone --depth 1 --branch V$SWIPL_VERSION --recurse-submodules https://github.com/SWI-Prolog/swipl-devel /swipl-devel -j100

# Build SWIPL
WORKDIR /swipl-devel/build.wasm

COPY /CMakeCache.txt /swipl-devel/build.wasm/CMakeCache.txt
RUN mkdir -p /swipl-devel/build.wasm
RUN cmake -DCMAKE_TOOLCHAIN_FILE=/emsdk/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DZLIB_LIBRARY=/zlib-$ZLIB_VERSION/libz.a \
  -DZLIB_INCLUDE_DIR=/zlib-$ZLIB_VERSION \
  -DGMP_ROOT=/gmp \
  -DINSTALL_DOCUMENTATION=OFF \
  -G Ninja ..
RUN ninja
