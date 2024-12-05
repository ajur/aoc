export { ascend as asc, descend as desc } from "@std/data-structures";

export const notNull = <T>(a: T | null | undefined): a is T => a !== null && a !== undefined;

// export const asc = ascend;
// export const desc = descend;

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
