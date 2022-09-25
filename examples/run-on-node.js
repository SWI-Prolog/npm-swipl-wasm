const path = require("path");
const SWIPL = require("swipl-wasm/swipl-node");

(async () => {
  const swipl = await SWIPL({ arguments: ["-q"] });
  console.log(swipl.prolog.query("member(X, [a, b, c]).").once().X);
})();
