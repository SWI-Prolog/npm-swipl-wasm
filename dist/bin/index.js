#!/usr/bin/env node

const { generateLoadedImageFile, generateImageFile } = require('../generateImage')
const path = require('path');

function showHelp() {
  console.log(`Usage: swipl-generate <input.pl> <output.js|output.ts> [--image-only]

Arguments:
  input.pl            Prolog source file or HTTP(S) URL
  output.js|output.ts Output JavaScript/TypeScript file

Options:
  --image-only Generate image data only
  --help       Show this help`);
}

function getPath(pth) {
  return pth.startsWith('http://') || pth.startsWith('https://')
    ? pth
    : path.join(process.cwd(), pth)
}

async function mainFunc() {
  let args = process.argv.slice(2);

  // Check for help flag or no arguments
  if (args.includes('--help') || args.includes('-h') || args.length === 0) {
    showHelp();
    return;
  }

  const fn = process.argv.includes('--image-only') ? generateImageFile : generateLoadedImageFile;
  args = args.filter(elem => elem !== '--image-only');

  if (args.length !== 2) {
    throw new Error('Expected exactly 2 inputs, input prolog (.pl) file and output file');
  }

  await fn(getPath(args[0]), getPath(args[1]));
}

mainFunc();
