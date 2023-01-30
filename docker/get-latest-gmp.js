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

main();
