declare global {
  interface Array<T> {
    inGroupOf(n: number): T[][];
    sum(): number;
    combinations(k: number): T[][];
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

export const fst = <T>(arr: [T, ...unknown[]]): T => arr[0];
export const snd = <T>(arr: [unknown, T, ...unknown[]]): T => arr[1];
export const last = <T>(arr: [...unknown[], T]): T => arr.at(-1) as T;
export const take = (n: number) => <T>(arr: T[]): T[] => arr.slice(0, n);
