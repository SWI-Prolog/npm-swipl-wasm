'use strict';

const assert = require('assert');

// Stub out the WASM bundle before loading generateImage so the tests don't
// require a full swipl build.
require('ts-node').register({ transpileOnly: true });
const Module = require('module');
const origResolve = Module._resolveFilename.bind(Module);
Module._resolveFilename = function (request, parent, isMain, options) {
  if (request === './swipl/swipl-bundle') return '__swipl_mock__';
  return origResolve(request, parent, isMain, options);
};
require.cache['__swipl_mock__'] = {
  id: '__swipl_mock__', filename: '__swipl_mock__', loaded: true,
  exports: async () => ({}),
};

const dns = require('dns/promises');
const fs = require('fs');

// Must be required *after* the stubs above are in place.
const { dereference, validateRemoteUrl } = require('../dist/generateImage');

describe('validateRemoteUrl', () => {
  let origLookup;

  beforeEach(() => {
    origLookup = dns.lookup;
  });

  afterEach(() => {
    dns.lookup = origLookup;
  });

  function stubLookup(addresses) {
    dns.lookup = async (_hostname, _opts) => addresses.map((a) => ({ address: a, family: a.includes(':') ? 6 : 4 }));
  }

  it('allows a public IPv4 address', async () => {
    stubLookup(['93.184.216.34']);
    await assert.doesNotReject(validateRemoteUrl(new URL('https://example.com')));
  });

  it('allows a public IPv6 address', async () => {
    stubLookup(['2606:2800:21f:cb07:6820:80da:af6b:8b2c']);
    await assert.doesNotReject(validateRemoteUrl(new URL('https://example.com')));
  });

  it('rejects 127.0.0.1 (IPv4 loopback)', async () => {
    stubLookup(['127.0.0.1']);
    await assert.rejects(validateRemoteUrl(new URL('http://127.0.0.1')), /internal network/);
  });

  it('rejects localhost (resolves to 127.0.0.1)', async () => {
    stubLookup(['127.0.0.1']);
    await assert.rejects(validateRemoteUrl(new URL('http://localhost')), /internal network/);
  });

  it('rejects 169.254.169.254 (cloud metadata endpoint)', async () => {
    stubLookup(['169.254.169.254']);
    await assert.rejects(validateRemoteUrl(new URL('http://169.254.169.254')), /internal network/);
  });

  it('rejects 10.x.x.x (RFC-1918)', async () => {
    stubLookup(['10.0.0.1']);
    await assert.rejects(validateRemoteUrl(new URL('http://10.0.0.1')), /internal network/);
  });

  it('rejects 172.16.x.x (RFC-1918)', async () => {
    stubLookup(['172.16.0.1']);
    await assert.rejects(validateRemoteUrl(new URL('http://172.16.0.1')), /internal network/);
  });

  it('rejects 192.168.x.x (RFC-1918)', async () => {
    stubLookup(['192.168.1.1']);
    await assert.rejects(validateRemoteUrl(new URL('http://192.168.1.1')), /internal network/);
  });

  it('rejects ::1 (IPv6 loopback)', async () => {
    stubLookup(['::1']);
    await assert.rejects(validateRemoteUrl(new URL('http://[::1]')), /internal network/);
  });

  it('rejects fc00::1 (IPv6 unique-local)', async () => {
    stubLookup(['fc00::1']);
    await assert.rejects(validateRemoteUrl(new URL('http://[fc00::1]')), /internal network/);
  });

  it('rejects fd12::1 (IPv6 unique-local fd range)', async () => {
    stubLookup(['fd12:3456::1']);
    await assert.rejects(validateRemoteUrl(new URL('http://[fd12:3456::1]')), /internal network/);
  });

  it('rejects fe80::1 (IPv6 link-local)', async () => {
    stubLookup(['fe80::1']);
    await assert.rejects(validateRemoteUrl(new URL('http://[fe80::1]')), /internal network/);
  });
});

describe('dereference', () => {
  let origLookup;
  let origReadFileSync;
  let origFetch;

  beforeEach(() => {
    origLookup = dns.lookup;
    origReadFileSync = fs.readFileSync;
    origFetch = global.fetch;
  });

  afterEach(() => {
    dns.lookup = origLookup;
    fs.readFileSync = origReadFileSync;
    global.fetch = origFetch;
  });

  function stubPublicLookup() {
    dns.lookup = async (_hostname, _opts) => [{ address: '93.184.216.34', family: 4 }];
  }

  it('reads a local file when allowLocalFiles is omitted (default true)', async () => {
    fs.readFileSync = (_path) => Buffer.from(':- true.');
    const result = await dereference('/tmp/test.pl');
    assert.ok(Buffer.isBuffer(result) || typeof result === 'string');
  });

  it('reads a local file when allowLocalFiles is explicitly true', async () => {
    fs.readFileSync = (_path) => Buffer.from(':- true.');
    const result = await dereference('/tmp/test.pl', { allowLocalFiles: true });
    assert.ok(Buffer.isBuffer(result) || typeof result === 'string');
  });

  it('rejects a local file when allowLocalFiles is false', async () => {
    await assert.rejects(
      dereference('/tmp/test.pl', { allowLocalFiles: false }),
      /Local file access is disabled/,
    );
  });

  it('fetches a public HTTP URL', async () => {
    stubPublicLookup();
    global.fetch = async (_url) => ({ text: async () => ':- true.' });
    const result = await dereference('http://example.com/test.pl');
    assert.strictEqual(result, ':- true.');
  });

  it('fetches a public HTTPS URL', async () => {
    stubPublicLookup();
    global.fetch = async (_url) => ({ text: async () => ':- true.' });
    const result = await dereference('https://example.com/test.pl');
    assert.strictEqual(result, ':- true.');
  });

  it('rejects a remote URL pointing to 127.0.0.1', async () => {
    dns.lookup = async (_hostname, _opts) => [{ address: '127.0.0.1', family: 4 }];
    await assert.rejects(
      dereference('http://127.0.0.1/secret'),
      /internal network/,
    );
  });

  it('rejects a remote URL that resolves to a private IP (DNS-level block)', async () => {
    dns.lookup = async (_hostname, _opts) => [{ address: '192.168.1.100', family: 4 }];
    await assert.rejects(
      dereference('http://internal.corp/secret'),
      /internal network/,
    );
  });
});
