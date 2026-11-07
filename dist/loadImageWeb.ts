import SWIPL from './swipl/swipl-web-no-data';
import { loadImage } from './loadImage'

export default function(image: string | Buffer | Uint8Array) {
  return loadImage(image, SWIPL);
}
