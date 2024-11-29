
declare global {
  interface Object {
    toEntries<T>(): [keyof T, T[keyof T]][];
  }
}

Object.prototype.toEntries = function <T extends string | number | symbol, V>(this: Record<T, V>): [T, V][] {
  return Object.entries(this) as [T, V][];
};

export const key = <K extends PropertyKey>(k: K) => <O extends Record<K, unknown>>(o: O): O[K] => o[k];
