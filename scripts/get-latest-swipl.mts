import { updateTag } from "./util.mts";

updateTag({
  owner: 'SWI-Prolog',
  repo: 'swipl-devel',
  getVersion: (tag) => /^V\d+.\d+.\d+$/.test(tag.name) ? tag.name.slice(1) : undefined,
  entry: 'swipl',
});
