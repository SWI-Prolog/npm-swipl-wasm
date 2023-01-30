/* eslint-disable import/no-extraneous-dependencies, no-console */
// A script for updating the version reference to eye
const fs = require('fs');
const path = require('path');

(async () => {
  const res = (await fetch('https://api.github.com/repos/madler/zlib/releases/latest'));
  if (res.status === 200) {
    const pkg = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')).toString());
    const tag = await res.json();
    pkg.config.zlib.version = tag.name.slice(5);
    fs.writeFileSync(
      path.join(__dirname, '..', 'package.json'),
      `${JSON.stringify(pkg, null, 2)}\n`,
    );
  } else {
    console.error(await res.text());
    process.exit(1);
  }
})();
