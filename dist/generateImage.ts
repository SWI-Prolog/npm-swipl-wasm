/// <reference types="emscripten" />

import SWIPL from './swipl/swipl-bundle';
import fs from 'fs';

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

function isForbiddenHost(hostname: string): boolean {
  const host = hostname.toLowerCase();
  if (host === 'localhost' || host === '::1' || host === '169.254.169.254') return true;
  const ipv4 = host.match(/^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/);
  if (ipv4) {
    const [a, b] = [parseInt(ipv4[1], 10), parseInt(ipv4[2], 10)];
    return a === 127 || a === 10 || a === 0 || (a === 169 && b === 254) ||
      (a === 172 && b >= 16 && b <= 31) || (a === 192 && b === 168);
  }
  return false;
}

function dereference(prologPath: string): Promise<string> | Buffer {
  if (prologPath.startsWith('http://') || prologPath.startsWith('https://')) {
    const { hostname } = new URL(prologPath);
    if (isForbiddenHost(hostname)) {
      throw new Error(`Refusing to fetch prolog file from disallowed host: ${hostname}`);
    }
    return fetch(prologPath).then((res) => res.text());
  }
  return fs.readFileSync(prologPath);
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
