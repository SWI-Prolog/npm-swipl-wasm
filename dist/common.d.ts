/// <reference types="emscripten" />

/**
 * Represents a Prolog variable.
 */
export type PrologVar = {
  $t: "v";
  /**
   * Variable id.
   */
  v?: number;
};

/**
 * Represents a Prolog string.
 */
export type PrologString = {
  $t: "s";
  /**
   * String contents.
   */
  v: string;
};

/**
 * Represents a Prolog rational number.
 */
export type PrologRational = {
  $t: "r";
  /**
   * Numerator of the rational.
   */
  n: number | bigint;
  /**
   * Denominator of the rational.
   */
  d: string | bigint;
};

/**
 * Represents a Prolog compound term.
 */
export type PrologCompound = {
  $t: "t";
  /**
   * Functor of the compound.
   */
  functor: string;
  /**
   * Arguments of the compound.
   */
  arguments(): PrologTerm[];
  /**
   * N-th argument (0-based).
   */
  arg(n: number): PrologTerm | undefined;
  /**
   * Arity of the compound.
   */
  arity(): number;
};

/**
 * Represents a Prolog list.
 */
export type PrologList = {
  $t: "l";
  /**
   * Head of the list.
   */
  v: PrologTerm;
  /**
   * Tail of the list.
   */
  t?: PrologTerm;
};

/**
 * Represents a Prolog blob.
 */
export type PrologBlob = {
  $t: "b";
};

export type PrologTerm =
  | PrologVar
  | PrologString
  | PrologRational
  | PrologCompound
  | PrologList
  | PrologBlob
  | string
  | number
  | bigint
  | boolean
  | void
  | null;

export type PrologCallOptions = {
  /**
   * Module in which to call Goal.
   */
  module?: string;
  /**
   * Call as yieldable.
   */
  async?: boolean;
};

export type ResponseValue = Record<string, PrologTerm>;

/**
 * Return type of Query#next().
 */
export type QueryResponse = {
  done: boolean;
  error?: boolean;
  /**
   * Error message set for some errors.
   */
  message?: string;
  value?: null | ResponseValue;
};

/**
 * Query interface for opening new queries.
 */
export type Query = {
  /**
   * Fetches the next solution.
   */
  next(): QueryResponse;
  /**
   * Runs the query. If there is a solution then returns it.
   * Closes the query.
   */
  once(): QueryResponse | ResponseValue;
};

/**
 * Prolog-JavaScript interface of the Prolog instance.
 */
export type Prolog = {
  /**
   * Call a Prolog goal.  This function deals with many variations to
   * call Prolog.
   */
  call(goal: string, opts?: PrologCallOptions): any;

  /**
   * Call code while reclaiming possibly allocated term_t references.
   *
   * @param f function to be called.
   * @param persist if `false`, discard all binding created
   * within the scope of the frame.
   */
  with_frame<T>(f: (prolog: Prolog) => T, persist = false): T | false;

  /**
   * Get a reference to a predicate.
   *
   * @param name Name of the predicate.  If this is the only argument it
   * encodes module, name and arity as `[module:]name/arity`.
   * @param arity Arity of the predicate.
   * @param module Module to resolve the predicate.
   */
  predicate(name: string, arity: number | undefined, module = "user"): number;

  /**
   * Lookup a Prolog module by name.
   *
   * @param name Name of the module.
   */
  new_module(name: string): number;

  /**
   * Run a possibly long running goal and process its answers.
   *
   * @param goal Goal to run.
   * @param input Goal input.
   * @param callback Optional callback to process.
   * @return A promise that is resolved on completion and rejected on
   * a Prolog exception.
   */
  forEach(
    goal: string,
    input?: any,
    callback?: (prolog: Prolog, answer: any) => void
  ): Promise<any>;

  /**
   * Calls the goal while binding input arguments.
   *
   * @param goal Goal to run.
   * @param input Input variable bindings.
   */
  query(goal: string, input?: Record<string, PrologTerm>): Query;
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
