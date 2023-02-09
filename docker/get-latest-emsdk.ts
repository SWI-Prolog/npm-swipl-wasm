import { getAllTags, getPackage, isHigherVersion, savePackage, Tag } from './util';

async function main() {
  const data = await getAllTags({ owner: 'emscripten-core', repo: 'emsdk' });

  let bestElem: Tag | undefined;
  for (const elem of data) {
    if (/^\d+.\d+.\d+$/.test(elem.name) && (!bestElem || isHigherVersion(elem.name, bestElem.name))) {
      bestElem = elem
    }
  }

  const pkg = getPackage();

  // If a higher version exists, update the package.json
  if (bestElem && isHigherVersion(bestElem.name, pkg.config.emsdk.version)) {
    pkg.config.emsdk.version = bestElem.name;
    savePackage(pkg);
  }
}

main()
