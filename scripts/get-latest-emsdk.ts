import { updateTag } from './util';

updateTag({
  owner: 'emscripten-core',
  repo: 'emsdk',
  getVersion: (tag) => /^\d+.\d+.\d+$/.test(tag.name) ? tag.name : undefined,
  entry: 'emsdk',
});
