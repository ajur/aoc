declare global {
  interface Math {
    mod(a: number, n: number): number;
  }

  interface Number {
    digits(): number;
    lastNDigits(d: number): number;
    firstNDigits(d: number): number;
  }

  interface NumberConstructor {
    lastNDigits(d: number): (n: number) => number;
    firstNDigits(d: number): (n: number) => number;
  }
}

Math.mod = (a: number, n: number) => a - n * Math.floor(a / n);


Number.prototype.digits = function(this: number) {
  return Math.log10(this) + 1 | 0;
}

Number.prototype.lastNDigits = function(this: number, d: number) {
  return this % (10 ** d);
}

Number.prototype.firstNDigits = function(this: number, d: number) {
  return this / 10 ** Math.max(0, this.digits() - d) | 0;
}

Number.lastNDigits = (d: number) => (n: number) => n.lastNDigits(d);
Number.firstNDigits = (d: number) => (n: number) => n.firstNDigits(d);

export {};
