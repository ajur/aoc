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

export const ulog = async (target: unknown, did?: string) => {
  did ??= `ulogDid-${Math.floor(Math.random() * 100000)}`;
  if (isJupyter && typeof target === "string") {
    await Deno.jupyter.broadcast("display_data", {
      data: { "text/plain": target },
      metadata: {},
      transient: { display_id: did },
    });
    return async (msg: string) => {
      await Deno.jupyter.broadcast("update_display_data", {
        data: { "text/plain": msg },
        metadata: {},
        transient: { display_id: did },
      });
    }
  } else if (isJupyter) {
    await Deno.jupyter.display(target ?? '', {display_id: did});
    return async (trg: unknown) => await Deno.jupyter.display(trg, { display_id: did, update: true});
  } else if (isVerbose) {
    let lastMsgLen = 0;
    const logger = (trg: unknown, clear = true) => {
      console.log(clear ? (cpl(lastMsgLen + 1) + ed() + trg) : trg);
      if (typeof trg === "string") {
        lastMsgLen = 0;
        for (let i = 0; i < trg.length; ++i) lastMsgLen += (trg.charAt(i) === '\n' ? 1 : 0);
      }
    }
    logger(target, false);
    return logger;
  } else {
    return () => {};
  }
}
