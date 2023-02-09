import { getAllTags, getPackage, savePackage, isHigherVersion as _isHigherVersion, Tag } from "./util";

async function main() {
  const data = await getAllTags({ owner: 'SWI-Prolog', repo: 'swipl-devel' });

  let bestElem: Tag | undefined;
  for (const elem of data) {
    if (/^V\d+.\d+.\d+$/.test(elem.name) && (!bestElem || isHigherVersion(elem.name, bestElem.name))) {
      bestElem = elem
    }
  }

  const pkg = getPackage();

  // If a higher version exists, update the package.json
  if (bestElem && isHigherVersion(bestElem.name, pkg.config.swipl.version)) {
    pkg.config.swipl.version = bestElem.name;
    pkg.config.swipl.commit = bestElem.commit.sha;
    savePackage(pkg);
  }
}

function isHigherVersion(v1: string, v2: string) {
  return _isHigherVersion(v1.slice(1), v2.slice(1));
}

main()
