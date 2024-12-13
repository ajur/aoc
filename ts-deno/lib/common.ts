export const isJupyter = (() =>{
  try {
    return Deno.jupyter !== undefined;
  } catch (_) {
    return false;
  }
})();

export const isVerbose = Deno.args.indexOf('-v') >= 0 || Deno.args.indexOf('--verbose') >= 0;

export const notNull = <T>(a: T | null | undefined): a is T => a !== null && a !== undefined;
export const id = <T>(o: T): T => o;

export const $hash: unique symbol = Symbol("hash");

export type Hashable = {
  [$hash](): string | number;
};
