export function isHigherVersion(v1, v2) {
  const [major1, minor1, patch1] = v1.split('.').map(e => parseInt(e));
  const [major2, minor2, patch2] = v2.split('.').map(e => parseInt(e));

  return major1 > major2
    || ((major1 === major2) && minor1 > minor2)
    || ((major1 === major2) && (minor1 === minor2) && patch1 > patch2);
}

export function getPackage() {
  return JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')).toString())
}

export function savePackage(packageJson) {
  fs.writeFileSync(path.join(__dirname, '..', 'package.json'), `${JSON.stringify(packageJson, null, 2)}\n`);
}

export async function getAllTags(repo) {
  let i = 0;

  const data = []

  while (true) {
    const res = await fetch(`https://api.github.com/repos/${repo}/tags?page=${i++}&per_page=100`);

    if (res.status !== 200) {
      throw new Error(`Error fetching latest swipl tags: ${await res.text()}`)
    }

    const content = await res.json();

    data.push(...content)

    if (!Array.isArray(content) || content.length === 0)
      break;
  }

  return data;
}
