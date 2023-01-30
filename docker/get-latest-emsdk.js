const fs = require('fs');
const path = require('path');

async function main() {
  let i = 0;

  const data = []

  while (true) {
    const res = await fetch(`https://api.github.com/repos/emscripten-core/emsdk/tags?page=${i++}&per_page=100`);

    if (res.status !== 200) {
      throw new Error(`Error fetching latest swipl tags: ${await res.text()}`)
    }

    const content = await res.json();

    data.push(...content)

    if (!Array.isArray(content) || content.length === 0)
      break;
  }

  let bestElem;
  for (const elem of data) {
    if (/^\d+.\d+.\d+$/.test(elem.name) && (!bestElem || isHigherVersion(elem.name, bestElem.name))) {
      bestElem = elem
    }
  }

  const package = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')).toString());

  // If a higher version exists, update the package.json
  if (isHigherVersion(bestElem.name, package.config.emsdk.version)) {
    package.config.emsdk.version = bestElem.name;
    fs.writeFileSync(path.join(__dirname, '..', 'package.json'), `${JSON.stringify(package, null, 2)}\n`);
  }
}

function isHigherVersion(v1, v2) {
  const [major1, minor1, patch1] = v1.split('.').map(e => parseInt(e));
  const [major2, minor2, patch2] = v2.split('.').map(e => parseInt(e));

  return major1 > major2
    || ((major1 === major2) && minor1 > minor2)
    || ((major1 === major2) && (minor1 === minor2) && patch1 > patch2);
}

main()
