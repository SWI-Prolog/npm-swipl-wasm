import SWIPL from './max';

async function main() {
  const Module = await SWIPL();
  const res = Module.prolog.query('max(1, 2, 3).');
  console.log(res.next())
}

main();
