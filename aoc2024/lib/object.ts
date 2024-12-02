
declare global {
  interface Object {
    peekMe<T>(this: T, f: (t: T) => unknown): T;
    logMe<T>(this: T, msg?: string, json?: boolean): T;
    callOnMe<T, R>(this: T, f: (t: T) => R): R;
  }
}

Object.prototype.peekMe = function <T>(this: T, f: (t: T) => unknown): T {
  f(this);
  return this;
}
Object.prototype.logMe = function <T>(this: T, msg?: string, json = false): T {
  if (msg && !json) console.log(msg, this);
  if (msg && json) console.log(msg, JSON.stringify(this, null, 2));
  return this;
}
Object.prototype.callOnMe = function <T, R>(this: T, f: (t: T) => R): R {
  return f(this);
}

export const key = <K extends PropertyKey>(k: K) => <O extends Record<K, unknown>>(o: O): O[K] => o[k];
