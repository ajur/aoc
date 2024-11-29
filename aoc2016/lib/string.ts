
declare global {
  interface String {
    toInt(): number;
    toNum(): number;
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

export const asInt = (s: string): number => s.toInt();
export const asNum = (s: string): number => s.toNum();
