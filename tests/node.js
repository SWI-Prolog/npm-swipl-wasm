const assert = require("assert");
const path = require("path");

describe("SWI-Prolog WebAssembly on Node.js", () => {
  for (const [SWIPL, name] of [
    [require("../dist/swipl-node"), 'node'],
    [require("../dist/swipl/swipl-web"), 'web'],
    [require("../dist/swipl/swipl-bundle"), 'bundle']
  ]) {

    const addedParams = name === 'web' ? {
      locateFile: (name) => path.join(__dirname, '..', 'dist', 'swipl', name)
    } : {};

    it(`[${name}] ` + "should conform to certain typing", async () => {
      assert.strictEqual(typeof SWIPL, "function");
  
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
  
      assert.strictEqual(typeof swipl.FS, "object");
      assert.strictEqual(typeof swipl.FS.writeFile, "function");
  
      assert.strictEqual(typeof swipl.prolog, "object");
      assert.strictEqual(typeof swipl.prolog.call, "function");
      assert.strictEqual(typeof swipl.prolog.query, "function");
    });
  
    it(`[${name}] ` + "should run simple query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      assert.strictEqual(
        swipl.prolog.query("member(X, [a, b, c]).").once().X,
        "a"
      );
    });
  
    it(`[${name}] ` + "should run query with arguments", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      assert.strictEqual(
        swipl.prolog.query("member(X, Y).", { Y: ["a", "b", "c"] }).once().X,
        "a"
      );
    });
  
    it(`[${name}] ` + "should run failing query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      const response = swipl.prolog.query("true").once();
      assert.strictEqual(response.$tag, "bindings");
    });
  
    it(`[${name}] ` + "should run throwing query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      const response = swipl.prolog.query("throw(error(test, _))").once();
      assert.strictEqual(response.error, true);
    });
  
    it(`[${name}] ` + "should eval javascript", async () => {
      global.wasRun = false;
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      await swipl.prolog.forEach("js_run_script(Script)", {
        Script: `global.wasRun = true;`,
      });
      assert.strictEqual(global.wasRun, true);
    });
  
    it(`[${name}] ` + "should query SWI-Prolog version", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      const version = swipl.prolog
        .query("current_prolog_flag(version, Version)")
        .once().Version;
      assert.ok(version >= 80517);
    });
  
    it(`[${name}] ` + "should have predictable term conversion", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      const atom = swipl.prolog.query("X = atom").once().X;
      assert.strictEqual(atom, "atom");
    });
  }
});
