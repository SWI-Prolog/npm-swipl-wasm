const assert = require("assert");
const path = require("path");
const SWIPL = require("swipl-wasm/swipl");

describe("SWI-Prolog WebAssembly on Node.js", () => {
  it("should conform to certain typing", async () => {
    assert.strictEqual(typeof SWIPL, "function");

    const swiplModuleLocation = require.resolve("swipl-wasm/swipl");
    const swipl = await SWIPL({
      arguments: ["-q"],
      locateFile: (url) => {
        if (url === "swipl-web.data") {
          return path.join(path.dirname(swiplModuleLocation), "swipl-web.data");
        } else if (url === "swipl-web.wasm") {
          return path.join(path.dirname(swiplModuleLocation), "swipl-web.wasm");
        }
        return url;
      },
    });

    assert.strictEqual(typeof swipl.FS, "object");
    assert.strictEqual(typeof swipl.FS.writeFile, "function");

    assert.strictEqual(typeof swipl.prolog, "object");
    assert.strictEqual(typeof swipl.prolog.call, "function");
    assert.strictEqual(typeof swipl.prolog.query, "function");
  });

  it("should run simple query", async () => {
    const swiplModuleLocation = require.resolve("swipl-wasm/swipl");
    const swipl = await SWIPL({
      arguments: ["-q"],
      locateFile: (url) => {
        if (url === "swipl-web.data") {
          return path.join(path.dirname(swiplModuleLocation), "swipl-web.data");
        } else if (url === "swipl-web.wasm") {
          return path.join(path.dirname(swiplModuleLocation), "swipl-web.wasm");
        }
        return url;
      },
    });
    assert.strictEqual(
      swipl.prolog.query("member(X, [a, b, c]).").once().X,
      "a"
    );
  });
});
