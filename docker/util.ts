import fs from 'fs';
import path from 'path';
import { Octokit } from '@octokit/rest';

export function isHigherVersion(v1: string, v2: string) {
  const [major1, minor1, patch1] = v1.split('.').map(e => parseInt(e));
  const [major2, minor2, patch2] = v2.split('.').map(e => parseInt(e));

  return major1 > major2
    || ((major1 === major2) && minor1 > minor2)
    || ((major1 === major2) && (minor1 === minor2) && patch1 > patch2);
}

export function getPackage() {
  return JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')).toString())
}

export function savePackage(packageJson: any) {
  fs.writeFileSync(path.join(__dirname, '..', 'package.json'), `${JSON.stringify(packageJson, null, 2)}\n`);
}

export type Tag = Awaited<ReturnType<Octokit['repos']['listTags']>>['data'][0];

export async function getAllTags(options: { owner: string, repo: string }): Promise<Tag[]> {
  const octokit = new Octokit();
  const allTags: Tag[] = []

  for (let i = 1; true; i += 1) {
    const { data } = await octokit.repos.listTags({
      ...options,
      per_page: 100,
      page: i,
    });

    if (data.length === 0)
      return data;

    allTags.push(...data);
  }
}
