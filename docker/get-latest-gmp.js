const fs = require('fs');
const path = require('path');

async function main() {
  const downloads = await fetch('https://gmplib.org/download/gmp/');
  const text = await downloads.text();
  const versions = text.match(/\"gmp-\d+.\d+.\d+.tar.xz\"/g).map(elem => elem.slice(5, -8));

  let bestVersion;

  for (const version of versions) {
    if (!bestVersion || isHigherVersion(version, bestVersion))
      bestVersion = version;
  }

  const package = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')).toString());

  // If a higher version exists, update the package.json
  if (isHigherVersion(bestVersion, package.config.gmp.version)) {
    package.config.gmp.version = bestVersion;
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

main();
