const path = require("path");
const SWIPL = require("./swipl/swipl");

// Wraps swipl.js built by Emscripten to provide location for
// .data and .wasm file locations.

module.exports = (options) => {
  return SWIPL({
    ...options,
    locateFile: (url) => {
      if (url === "swipl-web.data") {
        return path.join(__dirname, "swipl", "swipl-web.data");
      } else if (url === "swipl-web.wasm") {
        return path.join(__dirname, "swipl", "swipl-web.wasm");
      }
      return url;
    },
  });
};
