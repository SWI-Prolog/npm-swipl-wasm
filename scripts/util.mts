import fs from 'fs';
import path from 'path';
import { Octokit } from '@octokit/rest';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import buildConfigData from '../build-config.json' with { type: 'json' };

// Function to mimic __dirname in ES modules
const __dirname = dirname(fileURLToPath(import.meta.url));

export interface BuildConfig {
  swipl: {
    version: string;
    commit: string;
    name: string;
  };
  emsdk: {
    version: string;
    commit: string;
    name: string;
  };
  zlib: {
    version: string;
  };
  pcre2: {
    version: string;
    commit: string;
    name: string;
  };
}

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

export function getBuildConfig(): BuildConfig {
  return buildConfigData as BuildConfig;
}

export function saveBuildConfig(buildConfig: BuildConfig) {
  const configPath = path.join(__dirname, '..', 'build-config.json');
  try {
    fs.writeFileSync(configPath, `${JSON.stringify(buildConfig, null, 2)}\n`, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to write build-config.json: ${(error as Error).message}`);
  }
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
      return allTags;

    allTags.push(...data);
  }
}

export interface IUpdateTagOptions {
  owner: string;
  repo: string;
  getVersion: (tag: Tag) => string | undefined;
  entry: string;
}

export async function updateTag(options: IUpdateTagOptions) {
  const data = await getAllTags(options);

  let bestElem: { version: string; commit: string; name?: string } | undefined;
  for (const elem of data) {
    const version = options.getVersion(elem);
    if (version && (!bestElem || isHigherVersion(version, bestElem.version))) {
      bestElem = {  version, commit: elem.commit.sha, name: elem.name };
    }
  }

  const buildConfig = getBuildConfig();

  // If a higher version exists, update the build-config.json
  if (bestElem && isHigherVersion(bestElem.version, buildConfig[options.entry].version)) {
    buildConfig[options.entry] = bestElem;
    saveBuildConfig(buildConfig);
  }
}
