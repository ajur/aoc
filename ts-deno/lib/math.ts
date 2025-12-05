declare global {
  interface Math {
    mod(a: number, n: number): number;
    gcd(a: number, b: number): number;
    lcm(...n: number[]): number;
    /** solve ax + by = c, dx + ey = f */
    linear2(a: number, b: number, c: number, d: number, e: number, f: number): [number, number];
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
Math.gcd = (a: number, b: number) => (b == 0) ? a : Math.gcd(b, a % b);
Math.lcm = (...n: number[]) => n.reduce((a,b) => a / Math.gcd(a, b) * b);

Math.linear2 = (a: number, b: number, c: number, d: number, e: number, f: number): [number, number] => {
  const det = a * e - b * d;
  return [
    (c * e - b * f) / det,
    (a * f - c * d) / det
  ];
};

Number.prototype.digits = function(this: number) {
  return (this === 0 ? 1 : (Math.log10(Math.abs(this)) + 1)) | 0;
}

Number.prototype.lastNDigits = function(this: number, d: number) {
  return Math.abs(this) % (10 ** d);
}

Number.prototype.firstNDigits = function(this: number, d: number) {
  return Math.abs(this) / (10 ** Math.max(0, this.digits() - d)) | 0;
}

Number.lastNDigits = (d: number) => (n: number) => n.lastNDigits(d);
Number.firstNDigits = (d: number) => (n: number) => n.firstNDigits(d);

export {}
