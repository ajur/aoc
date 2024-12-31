// %%
import { asInt, lognb } from '#lib';
import { assertEquals } from "@std/assert";
// %%

const sample = `
1
10
100
2024
`
const parse = (s: string) => s.trim().split('\n').map(asInt);
parse(sample)

// %%

// looking at some numbers, in binary after day17 ptsd
// changing operators to binary operators
lognb((16777216).toString(2).padStart(30), "P - pruning number");
lognb((217777216).toString(2).padStart(30), "A - some number");
lognb((217777216 % 16777216).toString(2).padStart(30), "A % P");
lognb((217777216 & 16777215).toString(2).padStart(30), "A & (P - 1)");

lognb((234234).toString(2).padStart(30), "B");
lognb((64).toString(2).padStart(30), "64");
lognb((234234 * 64).toString(2).padStart(30), "B * 64");
lognb((234234 << 6).toString(2).padStart(30), "B << 6  (6 = log2(64)");

lognb((32).toString(2).padStart(30), "32");
lognb(Math.floor(234234 / 32).toString(2).padStart(30), "floor(B / 32)");
lognb((234234 >> 5).toString(2).padStart(30), "B >> 5  (5 = log2(32)");

lognb((2048).toString(2).padStart(30), "2048");
lognb((234234 * 2048).toString(2).padStart(30), "B * 2048");
lognb((234234 << 11).toString(2).padStart(30), "B << 11  (11 = log2(2048))");

// %%
const nextSecret = (secret: number): number => {
  const a = ((secret << 6) ^ secret) & 16777215;
  const b = ((a >> 5) ^ a) & 16777215;
  const c = ((b << 11) ^ b) & 16777215;
  return c;
}

{
  const tst = [123, 15887950,16495136,527345,704524,1553684,12683156,11100544,12249484,7753432,5908254];
  const ns = Array.from({length: 10}).reduce((arr: number[], _, idx) => [...arr, nextSecret(arr[idx])], [123]);
  assertEquals(ns, tst);
}

// %%

const getNthSecret = (start: number, N: number) => {
  let out = start;
  let n = N;
  while (n > 0) {
    out = nextSecret(out);
    --n;
  }
  return out;
}
{
  for (const [n, n2000] of [[1, 8685429], [10, 4700978], [100, 15273692], [2024, 8667524]]) {
    assertEquals(getNthSecret(n, 2000), n2000);
  }
}

// %%
const solveA = (s: string) => parse(s)
  .map(n => getNthSecret(n, 2000))
  .sum();
assertEquals(solveA(sample), 37327623);

// %%
const data = await Deno.readTextFile('./data/day22.txt');
console.log('Sol A:', solveA(data));
