/// <reference types="emscripten" />

import type SWIPL_TYPE from './common';

export function loadImage(image: string | Buffer | Uint8Array, swipl: typeof SWIPL_TYPE) {
  return (options?: Partial<EmscriptenModule> | undefined) => swipl({
    ...options,
    arguments: ['-q', '-x', 'image.pvm'],
    // @ts-ignore
    preRun: [(module: SWIPLModule) => module.FS.writeFile('image.pvm', image)],
  });
}
