export { ascend as asc, descend as desc } from "@std/data-structures";
export * as tc from "@coven/terminal";

export const notNull = <T>(a: T | null | undefined): a is T => a !== null && a !== undefined;

export const eq = <T>(a: T) => (b: T) => a === b;
export const ne = <T>(a: T) => (b: T) => a !== b;
export const gt = <T>(a: T) => (b: T) => b > a;
export const lt = <T>(a: T) => (b: T) => b < a;

export const not = <Args extends any[]>(f: (...args: Args) => boolean) => (...args: Args): boolean => !f(...args);
export const and = <Args extends any[]>(...ff: Array<(...args: Args) => boolean>) => (...args: Args) => ff.reduce((v, f) => v && f(...args), true);

export const id = <T>(o: T): T => o;

export const pipe = (...fns: Array<(x: any) => any>) => (x: any) => fns.reduce((v, f) => f(v), x); // typing this is hard -_-
export const compose = (...fns: Array<(x: any) => any>) => (x: any) => fns.reduceRight((v, f) => f(v), x); // typing this is hard -_-
export const mapped = <T, U, R>(m: (t: T) => U, f: (...args: U[]) => R) => (...args: T[]) => f(...(args.map(m)))

export const timeout = async (n: number): Promise<void> => {
  await new Promise((resolve) => setTimeout(resolve, n));
}

export const isJupyter = (() =>{
  try {
    return Deno.jupyter !== undefined;
  } catch (_) {
    return false;
  }
})();
export const isVerbose = Deno.args.indexOf('-v') >= 0 || Deno.args.indexOf('--verbose') >= 0;

export const log = (...args: Parameters<typeof console.log>) =>
  (isJupyter || isVerbose) && console.log(...args);
export const lognb = (...args: Parameters<typeof console.log>) =>
  isJupyter && console.log(...args);

export const ulog = async (did: string, msg?: string) => {
  if (isJupyter) {
    await Deno.jupyter.broadcast("display_data", {
      data: { "text/plain": msg ?? '' },
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
  } else if (isVerbose) {
    const logger = (msg: string) => {
      console.clear();
      console.log(msg);
    }
    if (msg) logger(msg);
    return logger;
  }
}
