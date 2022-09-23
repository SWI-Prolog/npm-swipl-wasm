const path = require('path');
const SWIPL = require('swipl-wasm/swipl');

(async () => {
  // This helps to find the correct locations of the
  // .data and .wasm files.
  const swiplModuleLocation = require.resolve('swipl-wasm/swipl');
  const swipl = await SWIPL({
    arguments: ['-q'],
    locateFile: (url) => {
      // These are common with the web version.
      if (url === 'swipl-web.data') {
        return path.join(path.dirname(swiplModuleLocation), 'swipl-web.data');
      } else if (url === 'swipl-web.wasm') {
        return path.join(path.dirname(swiplModuleLocation), 'swipl-web.wasm');
      }
      return url;
    }
  });
  // See https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650/1
  console.log(swipl.prolog.query("member(X, [a, b, c]).").once().X);
})();
