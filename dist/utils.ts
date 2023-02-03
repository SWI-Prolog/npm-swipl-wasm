import SWIPL from './swipl/swipl-bundle';
import type SWIPL_TYPE from './common';
import type {  } from 'emscripten';

export async function generateImage(prolog: string): Promise<Uint8Array> {
  const Module = await SWIPL({
    arguments: ['-q', '-f', 'prolog.pl'],
    // @ts-ignore
    preRun: (module: SWIPLModule) => module.FS.writeFile('prolog.pl', prolog),
  });

  Module.prolog.query("qsave_program('prolog.pvm')").once();
  return Module.FS.readFile('prolog.pvm')
}

export async function loadImage(swipl: typeof SWIPL_TYPE) {
  return (options?: Partial<EmscriptenModule> | undefined) => swipl({
    ...options,
    arguments: ['-q', '-x', 'eye.pvm'],
    // @ts-ignore
    preRun: (module: SWIPLModule) => module.FS.writeFile('image.pvm', strToBuffer(EYE_PVM)),
  });
}
