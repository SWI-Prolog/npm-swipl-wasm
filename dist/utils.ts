import SWIPL, { SWIPLModule } from './swipl/swipl-bundle';

export async function generateImage(prolog: string): Promise<Uint8Array> {
  const Module = await SWIPL({
    arguments: ['-q', '-f', 'prolog.pl'],
    // @ts-ignore
    preRun: [(module: SWIPLModule) => module.FS.writeFile('prolog.pl', prolog)],
  });

  Module.prolog.query("main(['--image', 'image.pvm'])")

  return Module.FS.readFile('image.pvm')
}
