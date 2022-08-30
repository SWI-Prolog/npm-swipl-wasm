import SWIPL, { SwiplOptions, SwiplModule } from './swipl/swipl';
import { locateFile } from './locateFile';

export default function(args: SwiplOptions): Promise<SwiplModule> {
  return SWIPL({ locateFile, noInitialRun: true, arguments: ["-q"], ...args });
}

export { SwiplOptions, SwiplModule } from './swipl/swipl';
