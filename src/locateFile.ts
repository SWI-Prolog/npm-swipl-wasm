import path from 'path';

// This helps to find the correct locations of the
// .data and .wasm files.
const swiplModuleLocation = require.resolve("swipl-wasm/swipl");
export function locateFile (url: string) {
  // These are common with the web version.
  if (url === "swipl-web.data") {
    return path.join(path.dirname(swiplModuleLocation), "swipl-web.data");
  } else if (url === "swipl-web.wasm") {
    return path.join(path.dirname(swiplModuleLocation), "swipl-web.wasm");
  }
  return url;
}