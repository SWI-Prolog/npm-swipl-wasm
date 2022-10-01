/// <reference types="emscripten" />

export type Prolog = {
  // call_string(str: string): void;
  query(str: string): any;
};

/**
 * SWI-Prolog instance.
 */
export type SWIPLModule = {
  /**
   * Emscripten emulated file system interface.
   */
  FS: typeof FS;
  /**
   * Prolog interface.
   */
  prolog: Prolog;
};

/**
 * Factory function that creates a SWI-Prolog instance.
 */
declare function initSWIPL(
  options?: Partial<EmscriptenModule>
): Promise<SWIPLModule>;

export = initSWIPL;
