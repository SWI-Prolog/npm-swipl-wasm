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

## Build

The package can be built using npm or yarn. Please use yarn to
add new dependencies and update yarn.lock file. SWI-Prolog WebAssembly
version is currently built inside Docker with Emscripten.

## TODO

- More examples (how to bundle with webpack)
- Integrate with SWI-Prolog CI
- TypeScript types for Prolog.js (and the Query interface)
- Use EcmaScript modules as output:
  <https://github.com/emscripten-core/emscripten/issues/11792>

## License

Same as SWI-Prolog license, BSD simplified:
<https://github.com/SWI-Prolog/swipl-devel/blob/master/LICENSE>
