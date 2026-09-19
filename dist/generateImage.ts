/// <reference types="emscripten" />

import SWIPL from './swipl/swipl-bundle';
import fs from 'fs';
import dns from 'dns/promises';

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

export interface DereferenceOptions {
  /**
   * Allow reading local filesystem paths. Defaults to true for backwards
   * compatibility; will default to false in the next major version.
   */
  allowLocalFiles?: boolean;
}

// Matches private/loopback/link-local IPv4 and IPv6 addresses.
const PRIVATE_IP_RE = [
  /^127\./,                        // 127.0.0.0/8 loopback
  /^10\./,                         // 10.0.0.0/8 RFC-1918
  /^172\.(1[6-9]|2\d|3[01])\./,   // 172.16.0.0/12 RFC-1918
  /^192\.168\./,                   // 192.168.0.0/16 RFC-1918
  /^169\.254\./,                   // 169.254.0.0/16 link-local / cloud metadata
  /^0\./,                          // 0.0.0.0/8 unspecified
  /^::1$/,                         // ::1 IPv6 loopback
  /^fc/i,                          // fc00::/7 IPv6 unique-local
  /^fd/i,                          // fd00::/8 IPv6 unique-local
  /^fe8/i,                         // fe80::/10 IPv6 link-local
];

export async function validateRemoteUrl(url: URL): Promise<void> {
  const addresses = await dns.lookup(url.hostname, { all: true });
  for (const { address } of addresses) {
    if (PRIVATE_IP_RE.some((re) => re.test(address))) {
      throw new Error(`Requests to internal network addresses are not allowed: ${address}`);
    }
  }
}

export async function dereference(prologPath: string, options: DereferenceOptions = {}): Promise<string | Buffer> {
  if (prologPath.startsWith('http://') || prologPath.startsWith('https://')) {
    const url = new URL(prologPath);
    await validateRemoteUrl(url);
    return fetch(prologPath).then((res) => res.text());
  }
  if (options.allowLocalFiles === false) {
    throw new Error('Local file access is disabled');
  }
  return fs.readFileSync(prologPath);
}

export async function generateImageFile(prologPath: string, jsPath: string, options?: DereferenceOptions): Promise<void> {
  fs.writeFileSync(
    jsPath,
    await generateImageFileString(await dereference(prologPath, options)),
  );
}

export async function generateLoadedImageFile(prologPath: string, jsPath: string, options?: DereferenceOptions): Promise<void> {
  fs.writeFileSync(
    jsPath,
    await generateLoadedImageFileString(await dereference(prologPath, options)),
  );
}
