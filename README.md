# swipl-wasm

SWI-Prolog WebAssembly build as a NPM package. Please see this page
for ongoing progress and information:
<https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650>

## Build

The package can be built using npm or yarn. Please use yarn to
add new dependencies and update yarn.lock file.

SWI-Prolog WebAssembly version is currently built inside Docker
with Emscripten.

## TODO

- More examples
- Integrate with SWI-Prolog CI
- TypeScript types for Prolog.js (and the Query interface)
- Use EcmaScript modules as output:
  <https://github.com/emscripten-core/emscripten/issues/11792>

## License

Same as SWI-Prolog license, BSD simplified:
<https://github.com/SWI-Prolog/swipl-devel/blob/master/LICENSE>
