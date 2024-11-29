
declare global {
  interface String {
    toInt(): number;
    toNum(): number;
    trim(c?: string): string;
  }
}

String.prototype.toInt = function (this: string): number {
  const r = +this;
  if (Number.isNaN(r) || !Number.isInteger(r)) {
    throw new Error(`Cannot convert "${this}" to an integer`);
  }
  return r;
};
String.prototype.toNum = function (this: string): number {
  const r = +this;
  if (Number.isNaN(r)) {
    throw new Error(`Cannot convert "${this}" to a number`);
  }
  return r;
};

const originalTrim = String.prototype.trim;
String.prototype.trim = function(c?: string): string {
  if (c === undefined || c.length === 0) return originalTrim.call(this);
  let a = 0;
  let b = this.length;
  const n = c.length;
  while (this.startsWith(c, a)) a += n;
  while (this.endsWith(c, b)) b -= n;
  return this.substring(a, b);
}

export const asInt = (s: string): number => s.toInt();
export const asNum = (s: string): number => s.toNum();
