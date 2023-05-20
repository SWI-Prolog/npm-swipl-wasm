
import { updateTag } from './util';

updateTag({
  owner: 'PCRE2Project',
  repo: 'pcre2',
  // Since pcre2 only does major.minor we add .0 to make it the semver we like
  getVersion: (tag) => /^pcre2\-\d+.\d+$/.test(tag.name) ? tag.name.slice(6) + '.0' : undefined,
  entry: 'pcre2',
});
