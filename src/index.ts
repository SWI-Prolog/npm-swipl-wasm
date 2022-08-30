import SWIPL, { SwiplOptions, SwiplModule } from './swipl/swipl';
import { locateFile } from './locateFile';

export default function SWIPLIsomorphic(args: SwiplOptions): Promise<SwiplModule> {
  return SWIPL({ locateFile, noInitialRun: true, arguments: ["-q"], ...args });
}

/**
 * A utility function for executing queries
 * @param str The query
 * @returns The results object
 */
export async function query(str: string) {
  const swipl = await SWIPLIsomorphic({})

  // See https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650/1
  return swipl.prolog.query(str);
}

export { SwiplOptions, SwiplModule } from './swipl/swipl';
