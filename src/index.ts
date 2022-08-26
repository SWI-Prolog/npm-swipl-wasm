const defaultOverrides: Partial<EmscriptenModule> = {
  noInitialRun: true,
  arguments: [],
};

const defaultNodeOverrides: Partial<EmscriptenModule> = {
  locateFile: (url) => {
    if (url.endsWith(".data")) {
      return `${__dirname}/swipl/swipl-web.data`;
    } else if (url.endsWith(".wasm")) {
      return `${__dirname}/swipl/swipl-web.wasm`;
    }
    return url;
  },
};

export type SWIPLModule = {
  FS: typeof FS;
} & EmscriptenModule;

/**
 * Initializes the web version of SWI-Prolog. The
 * web version uses emulated virtual filesystem while accessing files.
 */
export const initializeWeb = async (
  moduleOverrides: Partial<EmscriptenModule> = {}
) => {
  const overrides = { ...defaultOverrides, ...moduleOverrides };
  const { default: SWIPL } = await import("./swipl/swipl-web");
  return SWIPL(overrides) as Promise<SWIPLModule>;
};

/**
 * Initializes the Node version of SWI-Prolog. The Node
 * version uses Node.js filesystem APIs while accessing files.
 */
export const initializeNode = async (
  moduleOverrides: Partial<EmscriptenModule> = {}
) => {
  const overrides = {
    ...defaultOverrides,
    ...defaultNodeOverrides,
    ...moduleOverrides,
  };
  const { default: SWIPL } = await import("./swipl/swipl");
  return SWIPL(overrides) as Promise<SWIPLModule>;
};
