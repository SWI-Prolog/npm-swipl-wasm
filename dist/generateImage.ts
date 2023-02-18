/// <reference types="emscripten" />

import SWIPL from './swipl/swipl-bundle';
import fs from 'fs';
import { fetch } from 'cross-fetch';

function Uint8ToString(u8a: Uint8Array) {
  const CHUNK_SZ = 0x8000;
  const c: string[] = [];
  for (let i = 0; i < u8a.length; i += CHUNK_SZ) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    c.push(String.fromCharCode.apply(null, u8a.subarray(i, i + CHUNK_SZ) as any));
  }
  return c.join('');
}

export async function generateImageBuffer(prolog: string | Buffer): Promise<Uint8Array> {
  const Module = await SWIPL({
    arguments: ['-q', '-f', 'prolog.pl'],
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    preRun: [(module: SWIPLModule) => { module.FS.writeFile('prolog.pl', prolog) }],
  });

  Module.prolog.query("qsave_program('prolog.pvm')").once();
  return Module.FS.readFile('prolog.pvm')
}

export async function generateImageString(prolog: string | Buffer): Promise<string> {
  return btoa(Uint8ToString(await generateImageBuffer(prolog)));
}

export async function generateImageFileString(prolog: string | Buffer): Promise<string> {
  return `export default "${await generateImageString(prolog)}"\n`;
}

export async function generateLoadedImageFileString(prolog: string | Buffer) {
  return 'import loadImage from "swipl-wasm/dist/loadImageDefault"\n' +
    'import strToBuffer from "swipl-wasm/dist/strToBuffer"\n\n' +
    `export default loadImage(strToBuffer("${await generateImageString(prolog)}"))\n`;
}

function dereference(prologPath: string): Promise<string> | Buffer {
  return (prologPath.startsWith('http://') || prologPath.startsWith('https://'))
    ? fetch(prologPath).then((res) => res.text())
    : fs.readFileSync(prologPath)
}

export async function generateImageFile(prologPath: string, jsPath: string): Promise<void> {
  fs.writeFileSync(
    jsPath,
    await generateImageFileString(await dereference(prologPath)),
  );
}

export async function generateLoadedImageFile(prologPath: string, jsPath: string): Promise<void> {
  fs.writeFileSync(
    jsPath,
    await generateLoadedImageFileString(await dereference(prologPath)),
  );
}
