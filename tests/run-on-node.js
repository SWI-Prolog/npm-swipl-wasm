const { initializeNode } = require('../');

(async () => {
  const swipl = await initializeNode();
  console.log(swipl.prolog.query("member(X, [1, 2, 3]).").once().X);
})();
