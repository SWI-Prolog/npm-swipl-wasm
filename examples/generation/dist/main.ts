import SWIPL from './max';

async function main() {
  const Module = await SWIPL({ print: e => { console.log(e) } });
  const res = Module.prolog.query('max(1, 2, 3).');
  console.log(res.next())
}

main();
