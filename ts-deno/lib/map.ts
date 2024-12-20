import { $hash, Hashable } from './common.ts';

declare global {
  interface Map<K, V> {
    setdefault(k: K, v: V): V;
  }
}

Map.prototype.setdefault = function<K, V>(k: K, v: V): V {
  if (!this.has(k)) {
    this.set(k, v);
  }
  return this.get(k);
}

type HashFn<K> = (k: K) => string | number;

export class HashMap<K, V> implements Map<K, V> {
  #hash: HashFn<K>;
  #map: Map<string | number, [K, V]>;

  constructor(
    entries?: readonly (readonly [K, V])[] | null,
    hashFunc?: K extends Hashable ? never : HashFn<K>
  ) {
    this.#hash = hashFunc ?? ((k: K & Hashable) => k[$hash]()) as HashFn<K>;
    this.#map = new Map<string | number, [K, V]>(
      entries?.map(([k, v]) => [this.#hash(k), [k, v]])
    );
  }
  [Symbol.toStringTag]: string = "HashMap";

  clear(): void {
    this.#map.clear()
  }
  delete(key: K): boolean {
    return this.#map.delete(this.#hash(key))
  }
  forEach(callbackfn: (value: V, key: K, map: Map<K, V>) => void, thisArg?: unknown): void {
    this.#map.forEach(([k, v]: [K, V]) => callbackfn.call(thisArg, v, k, this));
  }
  get(key: K): V | undefined {
    return this.#map.get(this.#hash(key))?.[1];
  }
  has(key: K): boolean {
    return this.#map.has(this.#hash(key));
  }
  set(key: K, value: V): this {
    this.#map.set(this.#hash(key), [key, value]);
    return this;
  }
  get size() {
    return this.#map.size;
  }
  *entries(): MapIterator<[K, V]> {
    for (const v of this.#map.values()) {
      yield v;
    }
  }
  *keys(): MapIterator<K> {
    for (const [k] of this.#map.values()) {
      yield k;
    }
  }
  *values(): MapIterator<V> {
    for (const [_, v] of this.#map.values()) {
      yield v;
    }
  }
  setdefault(k: K, v: V): V {
    const hk = this.#hash(k);
    if (!this.#map.has(hk)) {
      this.#map.set(hk, [k, v]);
    }
    return this.#map.get(hk)![1];
  }
  [Symbol.iterator](): MapIterator<[K, V]> {
    return this.entries();
  }
  toString(): string {
    return 'HashMap(' + this.entries().map(([k, v]) => `${k}=${v}`).toArray().join(',') + ')';
  }
}
