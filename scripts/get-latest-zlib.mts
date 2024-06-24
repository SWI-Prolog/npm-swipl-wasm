/* eslint-disable import/no-extraneous-dependencies, no-console */
import { getPackage, savePackage } from "./util";

(async () => {
  const res = (await fetch('https://api.github.com/repos/madler/zlib/releases/latest'));
  if (res.status === 200) {
    const pkg = getPackage();
    const tag = await res.json();
    pkg.config.zlib.version = tag.name.slice(5);
    savePackage(pkg);
  } else {
    console.error(await res.text());
    process.exit(1);
  }
})();
