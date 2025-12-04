
import { asInt, lognb } from  "#lib";

// %%

const sample = `
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`;

const parse: (s: string) => [string, number][] = (s: string) => s.trim().split('\n')
  .map(l => [l.trim()[0], asInt(l.trim().substring(1))]);

parse(sample)
// %%

const solveA = (clicks: [string, number][], { start = 50, max = 100, log = false} = {}) => {
  let current = start;
  let zeros = 0;
  log && lognb("start at", start, "max at", max - 1)
  for (const [dir, num] of clicks) {
    const op = dir === 'L' ? -1 : 1;
    current = Math.mod(current + op * num, max);
    log && lognb(`after ${dir} ${num} at ${current}`);
    if (current === 0) {
      ++zeros;
    }
  }
  return zeros;
}


solveA(parse(sample), {log: true});

// %%

const data = await Deno.readTextFile("./data/day01.txt");

console.log("Sol A:", solveA(parse(data)))
// %%

// %%
const solveB = (clicks: [string, number][], { start = 50, max = 100, log = false} = {}) => {
  let current = start;
  let zeros = 0;
  log && lognb("start at", start, "max at", max - 1)
  for (const [dir, num] of clicks) {
    const op = dir === 'L' ? -1 : 1;
    for (let i = 0; i < num; i += 1) {
      current += op;
      if (current === -1) {
        current = 99;
      } else if (current === 100) {
        current = 0;
      }
      if (current === 0) {
        log && lognb(`during ${dir} ${num} passed 0`);
        ++zeros;
      }
    }
  }
  return zeros;
}

solveB(parse(sample), { log: true });
// %%
console.log("Sol B:", solveB(parse(data)))
