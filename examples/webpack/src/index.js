// Use swipl-wasm/ as the package name instead of
// ../../../ in the actual project.
import SWIPL from "../../../dist/swipl/swipl-web";
import wasmBinary from "../../../dist/swipl/swipl-web.wasm";
import wasmData from "../../../dist/swipl/swipl-web.data";

window.addEventListener("load", async () => {
  const swipl = await SWIPL({
    arguments: ["-q"],
    locateFile: (url, prefix) => {
      if (url === "swipl-web.data") {
        return wasmData;
      } else if (url === "swipl-web.wasm") {
        return wasmBinary;
      }
      return prefix + url;
    },
  });
  const query = "X = 'hello world from Prolog'";
  const solutionElement = document.getElementById("solution");
  const firstSolution = swipl.prolog.query(query).once().X;
  solutionElement.textContent = firstSolution;
});
