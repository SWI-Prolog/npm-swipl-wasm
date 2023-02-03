const image = require('../dist/utils');

// console.log(image.generateImage)

async function main() {
  const result = await image.generateImage(`
  find_max(X, Y, X) :- X >= Y, !.
  find_max(X, Y, Y) :- X < Y.
  
  find_min(X, Y, X) :- X =< Y, !.
  find_min(X, Y, Y) :- X > Y.
  `);
  console.log(result)
}

main();
