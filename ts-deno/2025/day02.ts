// %%

import { asInt, lognb } from "#lib";
import { assert } from "@std/assert";
// %%

const sample = `
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
`;

const parse: (s: string) => [number, number][] = (s: string) => s
  .replaceAll(/\s+/g, '')
  .split(',')
  .map(l => l.split('-').map(asInt) as [number, number]);

parse(sample)
// %%

const solveA = (clicks: [number, number][], { log = false} = {}) =>
  clicks.flatMap(([start, end]) => {
    const out: number[] = [];
    for (let i = start; i <= end; ++i) {
      const dn = i.digits();
      if (dn % 2 === 1) continue;
      const n = dn / 2;
      const l = i.firstNDigits(n);
      const r = i.lastNDigits(n);
      if (l === r) {
        log && lognb("found", i, 'repeats', l)
        out.push(i);
      }
    }
    return out;
  })
  .sum()


solveA(parse(sample), {log: true});

// %%

const data = await Deno.readTextFile("./data/day02.txt");

console.log("Sol A:", solveA(parse(data)))
// %%

const checkNumber = (num: number): boolean => {
  const sarr = num.toString().split('');
  const dn = sarr.length;
  for (let dnDiv = 2; dnDiv <= dn; ++dnDiv) {
    if (dn % dnDiv !== 0) continue;
    const n = dn / dnDiv;
    let ok = true;

    for (let n_nth = 1; n_nth < dnDiv; ++n_nth) {
      for (let k = 0; k < n; ++k) {
        if (sarr[k] !== sarr[n * n_nth + k]) {
          ok = false;
          break;
        }
      }
      if (!ok) break;
    }
    if (ok) return true;
  }
  return false;
}

assert(checkNumber(112112112112))
assert(checkNumber(323232))
assert(!checkNumber(101))

// %%

// this was supposed to work, but it breaks on zeroes, like 101 :/
const checkNumber_failedAttempt = (num: number): boolean => {
  lognb("--- checking", num);
  const dn = num.digits();
  lognb("dn", dn);
  for (let j = 2; j <= dn; ++j) {
    lognb("j", j);
    if (dn % j !== 0) continue;
    const n = dn / j;
    lognb('n', n)
    const l = num.firstNDigits(n);
    lognb('l', l)
    let r = num;
    let ok = true;
    for (let dc = 1; dc < j; ++dc) {
      r = r.lastNDigits(dn - n * dc);
      lognb('r', r)
      const lr = r.firstNDigits(n);
      lognb('lr', lr)
      if (l !== lr) {
        ok = false;
        break;
      }
    }
    if (ok) return true;
  }
  return false;
}

assert(checkNumber_failedAttempt(112112112112))
assert(checkNumber_failedAttempt(323232))
// assert(!checkNumber_failedAttempt(101))

// %%
const solveB = (clicks: [number, number][]) =>
  clicks.flatMap(([start, end]) => {
    const out: number[] = [];
    for (let i = start; i <= end; ++i) {
      if (checkNumber(i)) {
        out.push(i);
        lognb('found', i);
      }
    }
    return out;
  })
  .sum()

solveB(parse(sample));
// %%

console.log("Sol B:", solveB(parse(data)))
