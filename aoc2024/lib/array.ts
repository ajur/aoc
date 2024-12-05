import { notNull, asc } from "./misc.ts";

export type ArrayMapper<T, V> = (value: T, index: number, arr: T[]) => V;
export type ArrayPredicate<T> = ArrayMapper<T, boolean>;

declare global {
  interface Array<T> {
    inGroupOf(n: number): T[][];
    sum(): number;
    count(p?: ArrayPredicate<T>): number;
    combinations(k: number): T[][];
    isSorted(compareFn?: (a: T, b: T) => number): boolean;
    remove(from: number, to?: number): Array<T>;
    insert(at: number, ...values: T[]): Array<T>;
  }
}

Array.prototype.inGroupOf = function<T>(n: number): T[][] {
  const out: T[][] = [];
  for (let i = 0; i < this.length; i += n) {
      out.push(this.slice(i, i + n));
  }
  return out;
};

Array.prototype.sum = function(): number {
  let sum = 0;
  for (let i = 0; i < this.length; i++) {
      sum += +this[i];
  }
  return sum;
}

Array.prototype.count = function<T>(this: T[], p: ArrayPredicate<T> = notNull): number {
  let num = 0;
  for (let i = 0; i < this.length; ++i) {
      if (p(this[i], i, this)) {
        ++num;
      }
  }
  return num;
}

Array.prototype.combinations = function<T>(this: T[], k: number) {
  if (k < 0 || k > this.length) throw new Error("k should be in range <0, N> k=" + k + ' a=' + this);
  if (k === 0 || this.length === 0) return [];
  if (k === this.length || this.length === 1) return [this];
  if (k === 1) return this.map(v => [v]);
  const [head, ...tail] = this;
  return [
    ...tail.combinations(k - 1).map((aa: T[]) => [head, ...aa]),
    ...tail.combinations(k)
  ];
}

Array.prototype.isSorted = function<T>(this: T[], compareFn = asc): boolean {
  for (let i = 1; i < this.length; i++) {
      if (compareFn(this[i - 1], this[i]) > 0) {
        return false; // Return false if any pair is out of order
      }
    }
  return true;
}

Array.prototype.remove = function<T>(this: T[], from: number, to?: number): Array<T> {
  const count = (to ?? from) - from + 1;
  if (count < 1) return this;
  return this.splice(from, count);
}
Array.prototype.insert = function<T>(this: T[], at: number, ...values: T[]): Array<T> {
  this.splice(at, 0, ...values);
  return this;
}

export const fst = <T>(arr: [T, ...unknown[]]): T => arr[0];
export const snd = <T>(arr: [unknown, T, ...unknown[]]): T => arr[1];
export const last = <T>(arr: [...unknown[], T]): T => arr.at(-1) as T;
export const take = (n: number) => <T>(arr: T[]): T[] => arr.slice(0, n);


// %%

const f = (from: number, to?: number) => (to ?? from) - from + 1;
f(4, 2)
