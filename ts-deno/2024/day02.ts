
import { asInt } from  "#lib";
// %%

const sample = `
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9`

const parse = (s: string) => s.trim().split('\n').map(l => l.split(' ').map(asInt));

parse(sample)

// %%

const isSafe = (report: number[]): boolean => {
  if (report[0] > report[1]) {
    for (let i = 1; i < report.length; ++i) {
      const diff = report[i - 1] - report[i];
      if (diff < 1 || diff > 3) return false;
    }
  } else if (report[0] < report[1]) {
    for (let i = 1; i < report.length; ++i) {
      const diff = report[i] - report[i - 1];
      if (diff < 1 || diff > 3) return false;
    }
  } else {
    return false;
  }
  return true;
}

parse(sample).map(isSafe)

// %%

const solveA = (s: string) => parse(s).count(isSafe);

solveA(sample)
// %%

const data = await Deno.readTextFile('./data/day02.txt');

console.log("Sol A:", solveA(data))

// %%

const isSafeWithDamping = (report: number[]): boolean => {
  if (isSafe(report)) return true;
  return report.combinations(report.length - 1).some(isSafe);
}

const solveB = (s: string) => parse(s).count(isSafeWithDamping);

solveB(sample)
// %%

console.log("Sol B:", solveB(data));
