
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

export {}
