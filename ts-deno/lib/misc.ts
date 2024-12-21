import { isJupyter, isVerbose } from "./common.ts";
import { cpl, ed } from "./ansi.ts";

export const eq = <T>(a: T) => (b: T | undefined) => a === b;
export const ne = <T>(a: T) => (b: T | undefined) => a !== b;
export const gt = <T>(a: T) => (b: T | undefined) => b != null && b > a;
export const lt = <T>(a: T) => (b: T | undefined) => b != null && b < a;

export const not = <Args extends unknown[]>(f: (...args: Args) => boolean) => (...args: Args): boolean => !f(...args);
export const and = <Args extends unknown[]>(...ff: Array<(...args: Args) => boolean>) => (...args: Args) => ff.reduce((v, f) => v && f(...args), true);

export const mapped = <T, U, R>(m: (t: T) => U, f: (...args: U[]) => R) => (...args: T[]) => f(...(args.map(m)))
// deno-lint-ignore no-explicit-any
export const pipe = (...fns: Array<(x: any) => any>) => (x: any) => fns.reduce((v, f) => f(v), x); // typing this is hard -_-
// deno-lint-ignore no-explicit-any
export const compose = (...fns: Array<(x: any) => any>) => (x: any) => fns.reduceRight((v, f) => f(v), x); // typing this is hard -_-
export const peek = (msg?: string) => (o: unknown) => {
  msg ? console.log(msg, o) : console.log(o);
  return o;
}


export const memo = <A extends readonly unknown[], R>(f: (...args: A) => R, argsHasher?: (args: A) => string | number) => {
  const cache = new Map<string | number, R>();
  const hasher = argsHasher ?? ((args: A) => args.toString());

  return (...args: A): R => {
    const key = hasher(args);
    let out = cache.get(key);
    if (out === undefined) {
      out = f(...args);
      cache.set(key, out);
    }
    return out;
  }
}

export const timeout = async (n: number): Promise<void> => {
  await new Promise((resolve) => setTimeout(resolve, n));
}

export const log = (...args: Parameters<typeof console.log>) =>
  (isJupyter || isVerbose) && console.log(...args);
export const lognb = (...args: Parameters<typeof console.log>) =>
  isJupyter && console.log(...args);

let _lastDisplayId: string = '';
let _lastMsgLen: number = 0;
const _countLines = (s: string) => {
  let nls = 1;
  for (let i = 0; i < s.length; ++i) {
    if (s.charAt(i) === '\n') ++nls;
  }
  return nls;
}
export const display = async (target: unknown, did?: string) => {
  if (!did) {
    did = _lastDisplayId = `ulogDid-${Math.floor(Math.random() * 100000)}`;
  }
  if (isJupyter && typeof target === "string") {
    await Deno.jupyter.broadcast("display_data", {
      data: { "text/plain": target },
      metadata: {},
      transient: { display_id: did },
    });
  } else if (isJupyter) {
    await Deno.jupyter.display(target ?? '', {display_id: did});
  } else if (isVerbose) {
    const trg = typeof target === "string" ? target : (target?.toString() ?? (''+target));
    console.log(trg);
    _lastMsgLen = _countLines(trg);
  }
}

export const updateDisplay = async (target: unknown, did?: string) => {
  did ??= _lastDisplayId;
  if (isJupyter && typeof target === "string") {
    await Deno.jupyter.broadcast("update_display_data", {
      data: { "text/plain": target },
      metadata: {},
      transient: { display_id: did },
    });
  } else if (isJupyter) {
    await Deno.jupyter.display(target, { display_id: did, update: true});
  } else if (isVerbose) {
    const trg = typeof target === "string" ? target : (target?.toString() ?? (''+target));
    console.log(cpl(_lastMsgLen) + ed() + trg);
    _lastMsgLen = _countLines(trg);
  }
}
