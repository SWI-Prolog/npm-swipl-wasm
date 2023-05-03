import SWIPL from './max';

async function main() {
  const Module = await SWIPL();
  const res = Module.prolog.query('myMax(A, B, C).', { A: 1, B: 2 });
  if ((res.once() as { C: number }).C !== 2) {
    throw new Error('Failed to find max')
  }
}

main();
