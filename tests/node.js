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

  it("should run query with arguments", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    assert.strictEqual(
      swipl.prolog.query("member(X, Y).", { Y: ["a", "b", "c"] }).once().X,
      "a"
    );
  });

  it("should run failing query", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    const response = swipl.prolog.query("true").once();
    assert.strictEqual(response.$tag, "bindings");
  });

  it("should run throwing query", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    const response = swipl.prolog.query("throw(error(test, _))").once();
    assert.strictEqual(response.error, true);
  });

  it("should eval javascript", async () => {
    global.wasRun = false;
    const swipl = await SWIPL({ arguments: ["-q"] });
    await swipl.prolog.forEach("js_run_script(Script)", {
      Script: `global.wasRun = true;`,
    });
    assert.strictEqual(global.wasRun, true);
  });

  it("should query SWI-Prolog version", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    const version = swipl.prolog
      .query("current_prolog_flag(version, Version)")
      .once().Version;
    assert.ok(version >= 80517);
  });

  it("should have predictable term conversion", async () => {
    const swipl = await SWIPL({ arguments: ["-q"] });
    const atom = swipl.prolog.query("X = atom").once().X;
    assert.strictEqual(atom, "atom");
  });
});
