
export const notNull = <T>(a: T | null | undefined): a is T => a !== null && a !== undefined;

export const asc = (a: number, b: number) => a - b;
export const desc = (a: number, b: number) => b - a;

export const eq = <T>(a: T) => (b: T) => a === b;
export const ne = <T>(a: T) => (b: T) => a !== b;
export const gt = <T>(a: T) => (b: T) => b > a;
export const lt = <T>(a: T) => (b: T) => b < a;

export const not = <Args extends any[]>(f: (...args: Args) => boolean) => (...args: Args): boolean => !f(...args);

export const mapped = <T, U, R>(m: (t: T) => U, f: (...args: U[]) => R) => (...args: T[]) => f(...(args.map(m)))
