const assert = require("assert");
const path = require("path");

describe("SWI-Prolog WebAssembly on Node.js", () => {
  let log, warn, error;
  let logMessages = [], warnMessages = [], errorMessages = [];

  beforeEach(() => {
    log = console.log;
    warn = console.warn;
    error = console.error;
    console.log = (message) => logMessages.push(message);
    console.warn = (message) => warnMessages.push(message);
    console.error = (message) => errorMessages.push(message);
  });

  afterEach(() => {
    console.log = log;
    console.warn = warn;
    console.error = error;
    logMessages = [];
    warnMessages = [];
    errorMessages = [];
  });

  for (const [SWIPL, name] of [
    [require("../dist/swipl-node"), 'node'],
    [require("../dist/swipl/swipl-web"), 'web'],
    [require("../dist/swipl/swipl-bundle"), 'bundle'],
    [require("../dist"), '../dist'],
    [require(".."), '..'],
  ]) {

    const addedParams = name === 'web' ? {
      locateFile: (name) => path.join(__dirname, '..', 'dist', 'swipl', name)
    } : {};

    const packages = [
      'lists',
      'gensym',
      'system',
      'terms',
      'url',
      'charsio',
      'qsave',
      'base64',
      'date',
      'prolog_jiti',
      'sha',
      'dif',
      'semweb/turtle',
      'pcre',
      // 'uuid',
      // 'http/http_open',
    ];

    for (const package of packages) {
      it(`[${name}] ` + "should support the package " + package, async () => {
        const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-ignore
        swipl.onRuntimeInitialized();
        const importResult = swipl.prolog.query(`use_module(library(${package})).`).once().success;
        assert.strictEqual(importResult, true);
        assert.deepEqual(logMessages, []);
        assert.deepEqual(warnMessages, []);
        assert.deepEqual(errorMessages, []);
      });
    }

    it(`[${name}] ` + "should conform to certain typing", async () => {
      assert.strictEqual(typeof SWIPL, "function");
  
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
  
      assert.strictEqual(typeof swipl.FS, "object");
      assert.strictEqual(typeof swipl.FS.writeFile, "function");
  
      assert.strictEqual(typeof swipl.prolog, "object");
      assert.strictEqual(typeof swipl.prolog.call, "function");
      assert.strictEqual(typeof swipl.prolog.query, "function");
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should run simple query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      assert.strictEqual(
        swipl.prolog.query("member(X, [a, b, c]).").once().X,
        "a"
      );
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should run query with arguments", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      assert.strictEqual(
        swipl.prolog.query("member(X, Y).", { Y: ["a", "b", "c"] }).once().X,
        "a"
      );
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should run failing query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const response = swipl.prolog.query("true").once();
      assert.strictEqual(response.$tag, "bindings");
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should run throwing query", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const response = swipl.prolog.query("throw(error(test, _))").once();
      assert.strictEqual(response.error, true);

      assert.deepEqual(logMessages, ["Unknown error term: test"]);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should eval javascript", async () => {
      global.wasRun = false;
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      await swipl.prolog.forEach("js_run_script(Script)", {
        Script: `global.wasRun = true;`,
      });
      assert.strictEqual(global.wasRun, true);
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should query SWI-Prolog version", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const version = swipl.prolog
        .query("current_prolog_flag(version, Version)")
        .once().Version;
      assert.ok(version >= 80517);
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });

    it(`[${name}] ` + "should query SWI-Prolog Input", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const res = swipl.prolog
        .call("await('inputV', Input)", { async: true })
      assert.strictEqual("inputV", res.yield);
      const input = res.resume("testVal");
      assert.ok(input.done);
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  
    it(`[${name}] ` + "should have predictable term conversion", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const atom = swipl.prolog.query("X = atom").once().X;
      assert.strictEqual(atom, "atom");
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });

    it(`[${name}] ` + "should handle big ints", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      const atom = swipl.prolog.query("X is 555555555555555555555555555555555555555555555555555555").once().X;
      assert.strictEqual(atom, 555555555555555555555555555555555555555555555555555555n);
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });

    it(`[${name}] ` + "should do regex operations enabled by pcre2", async () => {
      const swipl = await SWIPL({ arguments: ["-q"], ...addedParams });
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      swipl.onRuntimeInitialized();
      assert.strictEqual(swipl.prolog.query("use_module(library(pcre)).").once().success, true);
      assert.strictEqual(swipl.prolog.query("re_match(\"^needle\"/i, \"Needle in a haystack\").").once().success, true);
      assert.strictEqual(swipl.prolog.query("re_match(\"^[0-9]{4}$\"/i, \"2023\").").once().success, true);
      assert.strictEqual(swipl.prolog.query("re_match(\"^[0-9]{4}$\"/i, \"202\").").once().success, false);
      assert.deepEqual(logMessages, []);
      assert.deepEqual(warnMessages, []);
      assert.deepEqual(errorMessages, []);
    });
  }
});
