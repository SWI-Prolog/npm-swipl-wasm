import { getPackage, isHigherVersion, savePackage } from './util';
import { fetch } from 'cross-fetch';

async function main() {
  const downloads = await fetch('https://gmplib.org/download/gmp/');
  const text = await downloads.text();
  const versions = text.match(/\"gmp-\d+.\d+.\d+.tar.xz\"/g)!.map((elem: string) => elem.slice(5, -8));

  let bestVersion: string | undefined;

  for (const version of versions) {
    if (!bestVersion || isHigherVersion(version, bestVersion))
      bestVersion = version;
  }

  const pkg = getPackage();

  // If a higher version exists, update the package.json
  if (bestVersion && isHigherVersion(bestVersion, pkg.config.gmp.version)) {
    pkg.config.gmp.version = bestVersion;
    savePackage(pkg);
  }
}

main();
