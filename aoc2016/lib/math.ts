
declare global {
  interface Math {
    mod(a: number, n: number): number;
  }
}

Math.mod = (a: number, n: number) => a - n * Math.floor(a / n);

export {};
