import SWIPL from "./swipl/swipl-web";

SWIPL({
  noInitialRun: true,
  arguments: ["swipl", "-x", "src/wasm-preload/boot.prc", "--nosignals"],
  locateFile: function (file: any) {
    console.log("locate " + file);
    //return path.join(__dirname, "wasm", "dist", file);
  },
  logReadFiles: true,
}).then(async (module: any) => {
  module.FS.writeFile("hello_world", "abc");
  module.prolog.call_string("consult(hello_world).");
  console.log("SWI-Prolog WebAssembly ready");
});
