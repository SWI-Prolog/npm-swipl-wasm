import SWIPL from './swipl/swipl-bundle-no-data';
import { loadImage } from './loadImage'

export default function(image: string | Buffer | Uint8Array) {
  return loadImage(image, SWIPL);
}

