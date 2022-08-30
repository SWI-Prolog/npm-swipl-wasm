export function locateFile (url: string) {
  if (url === "swipl-web.data") {
    return "/dist/swipl/swipl-web.data";
  } else if (url === "swipl-web.wasm") {
    return "/dist/swipl/swipl-web.wasm";
  }
  return url;
}
