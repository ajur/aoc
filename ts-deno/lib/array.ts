import { notNull } from "./common.ts";
import { ascend } from "@std/data-structures";

export type ArrayMapper<T, V> = (value: T, index: number, arr: T[]) => V;
export type ArrayPredicate<T> = ArrayMapper<T, boolean>;

declare global {
  interface Array<T> {
    inGroupOf(n: number): T[][];
    sum(): number;
    count(p?: ArrayPredicate<T>): number;
    combinations(k: number): T[][];
    permutations(k?: number, withRepeats?: boolean): IterableIterator<T[]>;
    isSorted(compareFn?: (a: T, b: T) => number): boolean;
    unique(eq?: (a: T, b: T) => boolean): T[];
    remove(from: number, to?: number): T[];
    insert(at: number, ...values: T[]): this;
    swap(i1: number, i2: number): this;
    shuffle(): this;
  }
}

Array.prototype.inGroupOf = function<T>(this: T[], n: number): T[][] {
  const out: T[][] = [];
  for (let i = 0; i < this.length; i += n) {
      out.push(this.slice(i, i + n));
  }
  return out;
};

Array.prototype.sum = function<T>(this: T[]): number {
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

Array.prototype.permutations = function*<T>(this: T[], k?: number, withRepeats = false): IterableIterator<T[]> {
  k ??= this.length;

  if (k === this.length && !withRepeats) {
    yield* generatePermutations(this);
  }
  throw new Error("NOT IMPLEMENTED");
}

Array.prototype.isSorted = function<T>(this: T[], compareFn = ascend): boolean {
  for (let i = 1; i < this.length; i++) {
      if (compareFn(this[i - 1], this[i]) > 0) {
        return false; // Return false if any pair is out of order
      }
    }
  return true;
}

Array.prototype.remove = function<T>(this: T[], from: number, to?: number): T[] {
  const count = (to ?? from) - from + 1;
  if (count < 1) return this;
  return this.splice(from, count);
}

Array.prototype.insert = function<T>(this: T[], at: number, ...values: T[]): T[] {
  this.splice(at, 0, ...values);
  return this;
}

Array.prototype.swap = function<T>(this: T[], i1: number, i2: number): T[] {
  const tmp = this[i1];
  this[i1] = this[i2];
  this[i2] = tmp;
  return this;
}

Array.prototype.unique = function<T>(eq?: (a: T, b: T) => boolean): T[] {
  eq ??= (a, b) => a === b;
  return this.filter((a, i) => this.findIndex((b) => eq(a, b)) === i);  // TODO maybe more efficient algo?
}

Array.prototype.shuffle = function<T>(this: T[]): T[] {
  // Fisher–Yates Shuffle from https://bost.ocks.org/mike/shuffle/
  let m = this.length;
  while (m) {  // While there remain elements to shuffle…
    const i = Math.floor(Math.random() * m--);  // Pick a remaining element…
    this.swap(i, m)// And swap it with the current element.
  }
  return this;
}

export const fst = <T>(arr: [T, ...unknown[]]): T => arr[0];
export const snd = <T>(arr: [unknown, T, ...unknown[]]): T => arr[1];
export const last = <T>(arr: [...unknown[], T]): T => arr.at(-1) as T;
export const take = (n: number) => <T>(arr: T[]): T[] => arr.slice(0, n);

export type RepeatMapper<T> = (idx: number) => T;
export const repeat = <T>(a: T | RepeatMapper<T>, n: number): T[] => Array.from({length: n}, (_, i) =>
  typeof a === "function" ? (a as RepeatMapper<T>)(i) : a);

function* generatePermutations<T>(ar: T[], k?: number): Generator<T[], void, unknown> {
  // https://en.wikipedia.org/wiki/Heap%27s_algorithm
  k ??= ar.length;
  if (k === 1) {
    yield [...ar];
  } else {
    yield* generatePermutations(ar, k - 1);

    for (let i = 0; i < k - 1; ++i) {
      ar.swap(k % 2 ? 0 : i, k - 1);
      yield* generatePermutations(ar, k - 1);
    }
  }
}
