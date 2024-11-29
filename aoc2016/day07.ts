// %%

import { assertEquals } from "@std/assert";
import { not, snd } from "./lib/index.ts";

const sample = `
abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn
`;

// %%

type Seq = [val: string, inBrackets: boolean];
type IPv7 = Seq[];

const parseLine = (line: string): IPv7 => {
  const out: IPv7 = [];
  let i = 0;
  let inBr = false;
  while (i < line.length) {
    const next = line.indexOf(inBr ? ']' : '[', i);
    const v = line.slice(i, next < 0 ? undefined : next);
    out.push([v, inBr]);
    i += v.length + 1;
    inBr = !inBr;
  }
  return out;
};

const parse = (s: string): IPv7[] => s.trim().split('\n').map(parseLine);
parse(sample)

// %%

const hasABBA = (s: string): boolean => {
  for (let i = 3; i < s.length; ++i) {
    if (s[i] === s[i - 3] && s[i - 1] === s[i - 2] && s[i] !== s[i - 1]) {
      return true;
    }
  }
  return false;
}

const supportsTLS = (d: IPv7): boolean => d.filter(not(snd)).some(([v]) => hasABBA(v)) && d.filter(snd).every(([v]) => !hasABBA(v))

parse(sample).map(supportsTLS)

// %%

const solveA = (s: string): number => parse(s).count(supportsTLS);
assertEquals(solveA(sample), 2)
// %%

const data = await Deno.readTextFile('./data/day07.txt');

console.log("Sol A: ", solveA(data))
// %%

const findABAandGetBABs = (s: string): string[] => {
  const babs: string[] = [];
  for (let i = 2; i < s.length; ++i) {
    if (s[i] === s[i - 2] && s[i] !== s[i - 1]) {
      babs.push([s[i - 1], s[i], s[i - 1]].join(''));
    }
  }
  return babs;
}

const hasBAB = (bab: string, s: string) => s.indexOf(bab) >= 0;

const supportsSSL = (d: IPv7): boolean => {
  const babs: string[] = [];
  for(const s of d.filter(not(snd))) {
    babs.push(...findABAandGetBABs(s[0]));
  }
  for (const bab of babs) {
    for (const s of d.filter(snd)) {
      if (hasBAB(bab, s[0])) {
        return true;
      }
    }
  }
  return false;
}

const solveB = (s: string) => parse(s).count(supportsSSL);

assertEquals(3, solveB(`
aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb
`))
// %%

console.log("Sol B:", solveB(data));
