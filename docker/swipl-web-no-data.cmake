
# Create swipl-web-no-data.js + swipl-web-no-data.wasm
#
# Same packaging as swipl-web (external, streamable .wasm and a small
# MODULARIZE'd JS glue), but without `--preload-file`, so no
# swipl-web-no-data.data is produced and the glue never fetches one.
# Like swipl-bundle-no-data, the result cannot load the standard
# Prolog library and must be booted from a saved state
# (e.g. `-x image.pvm`).
#
# This file is appended to cmake/EmscriptenTargets.cmake of swipl-devel
# by the npm-swipl-wasm Docker build.  It reuses WASM_DIST_LINK_FLAGS,
# SWIPL_SRC, PREJS and POSTJS defined earlier in that file.  The block
# is guarded so it becomes a no-op if a future swipl-devel ships the
# target itself.
if(NOT TARGET swipl-web-no-data)
  set(WASM_NO_DATA_WEB_LINK_FLAGS
      -Wno-unused-main)
  join_list(WASM_NO_DATA_WEB_LINK_FLAGS_STRING " "
	    ${WASM_NO_DATA_WEB_LINK_FLAGS} ${WASM_DIST_LINK_FLAGS})
  add_executable(swipl-web-no-data ${SWIPL_SRC})
  set_target_properties(swipl-web-no-data PROPERTIES
			LINK_FLAGS "${WASM_NO_DATA_WEB_LINK_FLAGS_STRING}")
  target_link_libraries(swipl-web-no-data libswipl)
  set_property(TARGET swipl-web-no-data PROPERTY LINK_DEPENDS
	       ${POSTJS} ${PREJS})
endif()
