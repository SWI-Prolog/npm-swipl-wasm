const assert = require("assert");
const SWIPL = require("../dist/swipl-node");

describe("SWI-Prolog WebAssembly on Node.js", () => {
  it("should conform to certain typing", async () => {
    assert.strictEqual(typeof SWIPL, "function");

    const swipl = await SWIPL({ arguments: ["-q"] });

    assert.strictEqual(typeof swipl.FS, "object");
    assert.strictEqual(typeof swipl.FS.writeFile, "function");

    assert.strictEqual(typeof swipl.prolog, "object");
    assert.strictEqual(typeof swipl.prolog.call, "function");
    assert.strictEqual(typeof swipl.prolog.query, "function");
  });

  it("should run simple query", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    assert.strictEqual(
      swipl.prolog.query("member(X, [a, b, c]).").once().X,
      "a"
    );
  });
});
