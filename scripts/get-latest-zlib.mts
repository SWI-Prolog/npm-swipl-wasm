/* eslint-disable import/no-extraneous-dependencies, no-console */
import { getBuildConfig, saveBuildConfig } from "./util.mts";

(async () => {
  const res = (await fetch('https://api.github.com/repos/madler/zlib/releases/latest'));
  if (res.status === 200) {
    const buildConfig = getBuildConfig();
    const tag = await res.json();
    buildConfig.zlib.version = tag.name.slice(5);
    saveBuildConfig(buildConfig);
  } else {
    console.error(await res.text());
    process.exit(1);
  }
})();
