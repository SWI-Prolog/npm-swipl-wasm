import type { FS, EmscriptenModule } from 'emscripten';

export interface Prolog {
  // call_string(str: string): void;
  query(str: string): any;
}

export interface SwiplModule {
  FS: FS;
  prolog: Prolog;
}

export type SwiplOptions = EmscriptenModule;

declare function SWIPL(options: Partial<EmscriptenModule>): Promise<SwiplModule>;

export default SWIPL;
