
import { asInt, transpose } from  './lib/index.ts';
// %%

const sample = `
  3   4
  4   3
  2   5
  1   3
  3   9
  3   3`;

const parse = (s: string) => s.trim().split('\n')
  .map(l => l.trim().split(/\s+/).map(asInt)).callOnMe(transpose).map(a => a.sort())

parse(sample)
// %%

const solveA = (lists: number[][]) => transpose(lists).map(([a, b]) => Math.abs(a - b)).sum();
solveA(parse(sample))

// %%

const data = await Deno.readTextFile("./data/day01.txt");

console.log("Sol A:", solveA(parse(data)))
// %%

const similarityScore = ([left, right]: number[][]): Map<number, number> => {
  const scores = new Map<number, number>();
  for (const num of left) {
    if (scores.has(num)) continue;
    const fst = right.indexOf(num);
    if (fst < 0) {
      scores.set(num, 0);
      continue;
    }
    const lst = right.lastIndexOf(num);
    scores.set(num, lst - fst + 1);
  }
  return scores;
}

const solveB = ([left, right]: number[][]) => {
  const scores = similarityScore([left, right]);
  return left.map(num => num * scores.get(num)!).sum();
}

solveB(parse(sample));
// %%
console.log("Sol B:", solveB(parse(data)))
