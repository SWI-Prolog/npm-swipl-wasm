#!/usr/bin/env node
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const buildConfig = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'build-config.json'), 'utf-8'));

// Export as environment variables for the build script
console.log(`SWIPL_VERSION=${buildConfig.swipl.version}`);
console.log(`SWIPL_COMMIT=${buildConfig.swipl.commit}`);
console.log(`SWIPL_NAME=${buildConfig.swipl.name}`);
console.log(`EMSDK_VERSION=${buildConfig.emsdk.version}`);
console.log(`EMSDK_COMMIT=${buildConfig.emsdk.commit}`);
console.log(`EMSDK_NAME=${buildConfig.emsdk.name}`);
console.log(`ZLIB_VERSION=${buildConfig.zlib.version}`);
console.log(`PCRE2_VERSION=${buildConfig.pcre2.version}`);
console.log(`PCRE2_COMMIT=${buildConfig.pcre2.commit}`);
console.log(`PCRE2_NAME=${buildConfig.pcre2.name}`);
