

export type ValueToNumberMapper<T> = (a: T) => number;
export type Comparator<T> = (a: T, b: T) => number;

export const asc = (a: number, b: number) => a - b;
export const desc = (a: number, b: number) => b - a;

export const eq = <T>(a: T) => (b: T) => a === b;
export const ne = <T>(a: T) => (b: T) => a !== b;
export const gt = <T>(a: T) => (b: T) => b > a;
export const lt = <T>(a: T) => (b: T) => b < a;

export type FnAnyToBoolean = (...args: any[]) => boolean;
export const not = (f: FnAnyToBoolean) => (...args: Parameters<FnAnyToBoolean>) => !f(...args);
export const and = (...ff: FnAnyToBoolean[]) => (...args: Parameters<FnAnyToBoolean>) =>
  ff.reduce((v: boolean, f: FnAnyToBoolean) => v && f(...args), true);


export const mod = (a: number, n: number) => a - n * Math.floor(a / n);

export type Vec = VecObject | VecTuple;
export type VecObject = {x: number, y: number};
export type VecTuple = [x: number, y: number];

const isVecTuple = (v: Vec): v is VecTuple => Array.isArray(v);

export const vx = (v: Vec) => isVecTuple(v) ? v[0] : v.x;
export const vy = (v: Vec) => isVecTuple(v) ? v[1] : v.y;
export const manhattan = (a: Vec, b: Vec): number => Math.abs(vx(b) - vx(a)) + Math.abs(vy(b) - vy(a));

const VecTupleConstructor: new (...args: VecTuple) => VecTuple = Array as any;

export class Vector extends VecTupleConstructor implements VecObject, VecTuple {
  constructor(x: number | Vec, y?: number) {
    super(
      typeof x === 'number' ? x : isVecTuple(x) ? x[0] : x.x,
      typeof x === 'number' ? y ?? x : isVecTuple(x) ? x[1] : x.y
    );
  }
  public get x() { return this[0]; }
  public set x(v: number) { this[0] = v; }
  public get y() { return this[1]; }
  public set y(v: number) { this[1] = v; }
  public eq(v: Vec) {
    return vx(v) === this.x && vy(v) === this.y;
  }
}

export type ArrayMapper<T, V> = (value: T, index: number, arr: T[]) => V;
export type ObjectMapper<T, V> = (value: T, key: string, obj: Record<string, T>) => V;
export type GridMapper<T, V> = (value: T, vec: Vector, arr: T[][]) => V;

export function gmap<T, V>(p: GridMapper<T, V>): (data: T[][]) => V[][];
export function gmap<T, V>(p: GridMapper<T, V>, data: T[][]): V[][];
export function gmap<T, V>(p: GridMapper<T, V>, g?: T[][]) {
  if (!g) return (g: T[][]) => gmap(p, g);
  return g.map((row, rowIdx) => row.map((val: T, colIdx: number) => p(val, new Vector(colIdx, rowIdx), g)));
}

export function map<T, V>(p: ArrayMapper<T, V>): (data: T[]) => V[];
export function map<T, V>(p: ArrayMapper<T, V>, data: T[]): V[];
export function map<T, V>(p: ObjectMapper<T, V>): (data: Record<string, T>) => Record<string, V>;
export function map<T, V>(p: ObjectMapper<T, V>, data: Record<string, T>): Record<string, V>;
export function map(p: any, g?: any) {
  if (!g) return (g: any) => map(p, g);
  if (Array.isArray(g)) {
    return g.map(p);
  } else {
    return Object.fromEntries(Object.entries(g).map(([key, val]) => [key, p(val, key, g)]));
  }
}


export type ArrayPredicate<T> = ArrayMapper<T, boolean>;
export type ObjectPredicate<T> = ObjectMapper<T, boolean>;
export type GridPredicate<T> = GridMapper<T, boolean>;

export function filter<T>(predicate: ArrayPredicate<T>): (data: T[]) => T[];
export function filter<T>(predicate: ObjectPredicate<T>): (data: Record<string, T>) => Partial<Record<string, T>>;
export function filter(predicate: any) {
  return (data: any) => {
    if (Array.isArray(data)) {
      return data.filter(predicate);
    } else {
      return Object.fromEntries(Object.entries(data).filter(([k, v]) => predicate(v, k, data)));
    }
  };
}


export function count<T>(ar: T[] | T[][]): number;
export function count<T>(p: ArrayPredicate<T>, ar: T[]): number;
export function count<T>(p: ArrayPredicate<T>): (ar: T[]) => number;
export function count<T>(p: GridPredicate<T>, g: T[][]): number;
export function count<T>(p: GridPredicate<T>): (g: T[][]) => number;
export function count<T>(p: any, g?: any) {
  if (!g && Array.isArray(p)) {
    return count(notNull, p);
  } else if (!g) {
    return (_g: any) => count(p, _g);
  } else if (Array.isArray(g)) {
    if (Array.isArray(g[0])) {
      let num = 0;
      for (let r = 0; r < g.length; ++r) {
        for (let c = 0; c < g[r].length; ++c) {
          if (p(g[r][c], new Vector(c, r), g)) {
            ++num;
          }
        }
      }
      return num;
    } else {
      let num = 0;
      for (let i = 0; i < g.length; ++i) {
          if (p(g[i], i, g)) {
            ++num;
          }
      }
      return num;
    }
  }
  throw new Error("Unexpected arguments");
}

export type CreateGridMapper<T> = (x: number, y: number) => T;
export const createGrid = <T>(w: number, h: number, a: T | CreateGridMapper<T>): T[][] => {
  return Array.from({length: h}, (_, y) =>
    Array.from({length: w}, (_, x) =>
      (typeof a === "function") ? (a as CreateGridMapper<T>)(x, y) : a))
}
export const gridFromString = <T>(str: string, mapper?: (s: string) => T, sepCol = '', sepRow = '\n'): T[][] => str.split(sepRow).map(pipe(split(sepCol), mapper ? map(mapper) : id))

export function indexOf<T>(p: GridPredicate<T>): (data: T[][]) => Vector | undefined;
export function indexOf<T>(p: ArrayPredicate<T>): (data: T[]) => number | undefined;
export function indexOf<T>(p: ObjectPredicate<T>): (data: Record<string, T>) => string | undefined;
export function indexOf(p: any) {
  return (data: any) => {
    if (Array.isArray(data)) {
      if (Array.isArray(data[0])) {
        for (let r = 0; r < data.length; ++r) {
          const c = data[r].findIndex((v: any, i: number) => p(v, new Vector(i, r), data));
          if (c >= 0) return new Vector(c, r);
        }
      } else {
        const idx = data.findIndex((v: any, i: number) => p(v, i, data));
        if (idx >= 0) return idx;
      }
    } else {
      for (const [k, v] of Object.entries(data)) {
        if (p(v, k, data)) {
          return k;
        }
      }
    }
  }
}

export function at<T>(v: Vec, g: T[][]): T | undefined;
export function at<T>(v: Vec): (g: T[][]) => T | undefined;
export function at<T>(x: number, y: number, g: T[][]): T | undefined;
export function at<T>(v: number, y: number): (g: T[][]) => T | undefined;
export function at<T>(v: number, g: T[]): T | undefined;
export function at<T>(v: number): (g: T[]) => T | undefined;
export function at<T>(a: any, b?: any, c?: any) {
  if (typeof a === "number" && typeof b === "number") {
    return Array.isArray(c) ? c[b]?.[a] : ((g: T[][]) => g[b]?.[a]);
  } else if (typeof a === "number") {
    return Array.isArray(b) ? b[a] : ((g: T[]) => g[a]);
  } else {
    return Array.isArray(b) ? b[vy(a)]?.[vx(a)] : ((g: T[][]) => g[vy(a)]?.[vx(a)]);
  }
}

export function find<T>(p: GridPredicate<T>, data: T[][]): T | undefined;
export function find<T>(p: GridPredicate<T>): (data: T[][]) => T | undefined;
export function find<T>(p: ArrayPredicate<T>, data: T[]): T | undefined;
export function find<T>(p: ArrayPredicate<T>): (data: T[]) => T | undefined;
export function find<T>(p: ObjectPredicate<T>, data: Record<string, T>): T | undefined;
export function find<T>(p: ObjectPredicate<T>): (data: Record<string, T>) => T | undefined;
export function find(p: any, data?: any): any {
  if (data === undefined) {
    return (d: any) => find(p, d);
  }
  if (Array.isArray(data)) {
    if (Array.isArray(data[0])) {
      for (let r = 0; r < data.length; ++r) {
        const c = data[r].findIndex((v: unknown, i: number) => p(v, new Vector(i, r), data));
        if (c >= 0) return data[r][c];
      }
    } else {
      const idx = data.findIndex((v: unknown, i: number) => p(v, i, data));
      if (idx >= 0) return data[idx];
    }
  } else {
    for (const [k, v] of Object.entries(data)) {
      if (p(v, k, data)) {
        return v;
      }
    }
  }
  return undefined;
}

export function getRow<T>(idx: number, g: T[][]): T[] | undefined;
export function getRow<T>(idx: number): (g: T[][]) => T[] | undefined;
export function getRow<T>(idx: number, g?: T[][]) {
  if (g === undefined) {
    return (g: T[][]) => getRow(idx, g);
  }
  if (idx >= 0 && idx < g.length) {
    return g[idx];
  }
}

export function getCol<T>(idx: number, g: T[][]): T[] | undefined;
export function getCol<T>(idx: number): (g: T[][]) => T[] | undefined;
export function getCol<T>(idx: number, g?: T[][]) {
  if (g === undefined) {
    return (g: T[][]) => getCol(idx, g);
  }
  if (idx >= 0 && idx < g[0].length) {
    return g.map(a => a[idx]);
  }
}

export function setRow<T>(idx: number, v: T[], g: T[][]): T[][];
export function setRow<T>(idx: number, v: T[]): (g: T[][]) => T[][];
export function setRow<T>(idx: number, v: T[], g?: T[][]) {
  if (g === undefined) {
    return (g: T[][]) => setRow(idx, v, g);
  }
  if (idx >= 0 && idx < g.length && g[idx].length === v.length) {
    for (let i = 0; i < v.length; ++i) {
      g[idx][i] = v[i];
    }
    return g;
  }
  throw new Error("index or value size out of bounds");
}

export function setCol<T>(idx: number, v: T[], g: T[][]): T[][];
export function setCol<T>(idx: number, v: T[]): (g: T[][]) => T[][];
export function setCol<T>(idx: number, v: T[], g?: T[][]) {
  if (g === undefined) {
    return (g: T[][]) => setCol(idx, v, g);
  }
  if (idx >= 0 && idx < g[0].length && g.length === v.length) {
    for (let i = 0; i < v.length; ++i) {
      g[i][idx] = v[i];
    }
    return g;
  }
  throw new Error("index or value size out of bounds");
}
