# swipl-wasm

SWI-Prolog WebAssembly build as a NPM package. Please see this page
for ongoing progress and information:
<https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650>

## Usage

In browser:

```html
<div id="solution"></div>
<script src="/dist/swipl/swipl-web.js"></script>
<script>
  (async () => {
    const swipl = await SWIPL({
      arguments: ["-q"],
      locateFile: (path) => {
        return `/dist/swipl/${path}`;
      },
    });
    const query = "member(X, [a, b, c]).";
    const solutionElement = document.getElementById("solution");
    const firstSolution = swipl.prolog.query(query).once().X;
    solutionElement.textContent = firstSolution;
  })();
</script>
```

The function `locateFile` will help the browser to find the necessary
files (`swipl-web.wasm` and `swipl-web.data`). In this case the files
should be served along with `swipl-web.js` under the `/dist/swipl`
directory in the web server.

You can run this example by executing `npm run test:serve-http` and
visiting <http://localhost:8080/examples/browser.html>.

In Nodejs:

```js
const swipl = await SWIPL({ arguments: ["-q"] });
console.log(swipl.prolog.query("member(X, [a, b, c]).").once().X);
```

You can run this example with `node examples/run-on-node.js`.

## Running JavaScript from Prolog

This uses `eval`:

```js
swipl.prolog
  .query("js_run_script(Script)", {
    Script: `console.log('hello')`,
  })
  .once();
```

## Using with Webpack

[Webpack](https://webpack.js.org/) is a JavaScript and resources
bundler for large-scale frontend projects.

There is an example Webpack project in `examples/webpack`. It uses
[Asset Modules](https://webpack.js.org/guides/asset-modules/) to "load"
necessary `.data` and `.wasm` files. The location of these files and then
fed to `locateFile` (see above).

The package `swipl-wasm` is linked into the example. In an actual project
you would declare `swipl-wasm` as a normal dependency.

To start the example:

```
cd examples/webpack
npm install
npm build
npm run server
```

and visit <http://127.0.0.1:8080>. You should see the message "Hello world from
Prolog".

## Build

The package can be built using npm or yarn. Please use yarn to
add new dependencies and update yarn.lock file. SWI-Prolog WebAssembly
version is currently built inside Docker with Emscripten.

## Versioning

The package uses its own versioning scheme using semver. It is
detached from the versioning of SWI-Prolog itself.

To get the underlying SWI-Prolog version:

```js
const swipl = await SWIPL({ arguments: ["-q"] });
const version = swipl.prolog
  .query("current_prolog_flag(version, Version)")
  .once().Version;
```

The version is returned as integer `10000 × Major + 100 × Minor + Patch`.

## TODO

- Integrate with SWI-Prolog CI
- TypeScript types for Prolog.js (and the Query interface)
- Use EcmaScript modules as output:
  <https://github.com/emscripten-core/emscripten/issues/11792>

## License

Same as SWI-Prolog license, BSD simplified:
<https://github.com/SWI-Prolog/swipl-devel/blob/master/LICENSE>
