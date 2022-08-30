import SWIPL, { SwiplOptions, SwiplModule } from './swipl';
import { locateFile } from './locateFile';

export default function(args: SwiplOptions): SwiplModule {
  return SWIPL({ locateFile, noInitialRun: true, arguments: ["-q"], ...args });
}
