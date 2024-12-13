
import { Vector, neighbours } from  "#lib";
// %%

const sample = `
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX`;

const parse = (s: string) => s.trim().split('\n');

const wordDirs = neighbours([0, 0]);
const words = (v: Vector) => wordDirs.map(dir => [v, v.add(dir), v.add(dir.mult(2)), v.add(dir.mult(3))]);
const XMAS = 'XMAS'.split('');

// %%

const isXMASforS = (ss: string[]) => (w: Vector[]) => {
  for (let i = 0; i < XMAS.length; ++i) {
    if (XMAS[i] !== ss[w[i].y]?.[w[i].x]) return false;
  }
  return true;
}

const findWords = (ss: string[]) => {
  const found: Vector[][] = [];
  const isXMAS = isXMASforS(ss);
  for (let y = 0; y < ss.length; ++y) {
    let x = ss[y].indexOf(XMAS[0]);
    while (x >= 0) {
      found.push(...words(new Vector(x, y)).filter(isXMAS));
      x = ss[y].indexOf(XMAS[0], x + 1);
    }
  }
  return found;
}

const solveA = (s: string) => findWords(parse(s)).length;
solveA(sample)
// %%
const data = await Deno.readTextFile('./data/day04.txt');
console.log('Sol A:', solveA(data));
// %%

const isX_MASforS = (s: string[]) => (va: Vector) => {
  const x = va.x, y = va.y;
  if (s[y]?.[x] !== 'A') return false;
  return ((s[y - 1]?.[x - 1] === 'M' && s[y + 1]?.[x + 1] === 'S')
      || (s[y - 1]?.[x - 1] === 'S' && s[y + 1]?.[x + 1] === 'M'))
      && ((s[y - 1]?.[x + 1] === 'M' && s[y + 1]?.[x - 1] === 'S')
      || (s[y - 1]?.[x + 1] === 'S' && s[y + 1]?.[x - 1] === 'M'));
}

const findX_MAS = (ss: string[]) => {
  let found = 0;
  const isX_MAS = isX_MASforS(ss);
  for (let y = 0; y < ss.length; ++y) {
    let x = ss[y].indexOf('A');
    while (x >= 0) {
      if (isX_MAS(new Vector(x, y))) {
//         console.log(`found:
// ${ss[y - 1]?.[x - 1]} ${ss[y - 1]?.[x + 1]}
//  ${ss[y]?.[x]}
// ${ss[y + 1]?.[x - 1]} ${ss[y + 1]?.[x + 1]}`);
        ++found;
      }
      x = ss[y].indexOf('A', x + 1);
    }
  }
  return found;
}

const solveB = (s: string) => findX_MAS(parse(s));
solveB(sample)
// %%

console.log('Sol B:', solveB(data));
