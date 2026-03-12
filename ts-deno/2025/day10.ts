import {} from "../lib/array.ts";
import { asInt } from "../lib/string.ts";
import { aStar } from "../lib/astar.ts";
import { lognb, log } from "../lib/misc.ts";
import highsLoader, { Highs } from "highs";
import { Grid } from "../lib/grid.ts";
const highs = await (highsLoader as unknown as () => Promise<Highs>)();
// %%

const sample =`
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
`;

type Machine = {
  target: string;
  buttons: number[][];
  joltage: number[];
};
type ParsedData = Machine[];
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => s.trim()
  .split('\n')
  .map(l => {
    const parts = l.split(' ');
    const t = parts.shift()!;
    const j = parts.pop()!;
    return {
      target: t.substring(1, t.length - 1), //.split('').map(asBool('#')),
      joltage: j.substring(1, j.length - 1).split(',').map(asInt),
      buttons: parts.map(ps => ps.substring(1, ps.length - 1).split(',').map(asInt))
    }
  });

lognb(parse(sample))
// %%

const pushButton = (n: boolean[], b: number[]): boolean[] => {
  const nn = [...n];
  for (const i of b) {
    nn[i] = !nn[i];
  }
  return nn;
}

// %%

const solveMachine = (m: Machine): number => {
  const sol = aStar({
    start: m.target.split('').map(_ => false),
    isEnd: (_p, h) => h === m.target,
    hash: (p) => p.map(v => v ? '#' : '.').join(''),
    nbs: (n) => m.buttons.map(b => pushButton(n, b)),
  });
  // lognb(sol);
  return sol[0][0];
}

lognb(solveMachine(parse('[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}')[0]))

// %%

const solveA = (pd: ParsedData) => pd.map(solveMachine).sum();

lognb(solveA(parse(sample)));

// %%

const data = await Deno.readTextFile("./data/day10.txt");

console.log("Sol A:", solveA(parse(data)))

// %%
//[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
// const PROBLEM = `Minimize
//  obj:
//     x0 + x1 + x2 + x3 + x4 + x5
// Subject To
//  c1: x4 + x5 = 3
//  c2: x1 + x5 = 5
//  c3: x2 + x3 + x4 = 4
//  c4: x0 + x1 + x3 = 7
// End`;

const buildProblemString = ({buttons, joltage}: Machine): string =>
`Minimize
obj:
  ${buttons.map((_, i) => 'x'+i).join(' + ')}
Subject To
  ${
    joltage.map((jv, ji) => `c${ji}: ${
      buttons.map((bn, bi) => [bn, bi] as [number[], number])
        .filter(([bn]) => bn.includes(ji))
        .map(([_, bi]) => 'x'+bi)
        .join(' + ')
    } = ${jv}`).join('\n  ')
  }
General
  ${buttons.map((_, i) => 'x'+i).join(' ')}
End
`

lognb(buildProblemString(parse(sample)[0]))

// %%

const solveMachineB = (m: Machine): number => highs.solve(buildProblemString(m)).ObjectiveValue;

const solveB = (pd: ParsedData) => pd.map(solveMachineB).sum();

lognb(solveB(parse(sample)))

// %%

console.log("Sol B:", solveB(parse(data)))

// %%

////////////////////////////////////////////////////////////////////////////////////////////
/// LETS LEAVE THE MAYHEM BELOW AS A WORNING...

/// ATTEMPT 1 -----------------------------------------------------------------------------
/// completly failed aStar, with an attempt of improving speed, but still failed - not good brute force method for sure

const pushJoltageButton = (n: number[], b: number[], c = 1): [number[], number] => {
  const nn = [...n];
  for (const i of b) {
    nn[i] -= c;
  }
  return [nn, c];
}

const filterBtns = ([bn]: [number[], number]): boolean => {
  for (let i = 0; i < bn.length; ++i) {
    if (bn[i] < 0) return false;
  }
  return true;
}

const _solveMachineB_failedAStar = (m: Machine): number => {
  const targetHash = m.joltage.map(_ => 0).join(',');

  const nbsDists = (n: number[]) => {
    const min = Math.min(...n.filter(n => n > 0));
    const halfMax = Math.floor(Math.max(...n) / 2);
    const halfMin = Math.max(1, Math.floor(min / 2));
    const btns =  [
      ...m.buttons.map(b => pushJoltageButton(n, b, min)),
      ...(min !== 1 ? m.buttons.map(b => pushJoltageButton(n, b, 1)) : []),
      ...(halfMin !== min ? m.buttons.map(b => pushJoltageButton(n, b, halfMin)) : []),
      ...(halfMax !== min ? m.buttons.map(b => pushJoltageButton(n, b, halfMax)) : []),
      ].filter(filterBtns);
    // console.log("from", n.join(','), "\nnext", btns.map(b => b[0].join(',')+'-len-'+b.at(-1)).join('\n  '));
    return btns;
  };

  lognb('solving', m)
  const sol = aStar({
    start: m.joltage,
    isEnd: (_p, h) => h === targetHash,
    hash: (p) => p.join(','),
    nbsDists,
    score: (n) => n.map(r => r * r).sum(),
    listener: (a, b) => a !== "skipped" && console.log(a, b.hash, b.dist, b.factor)
  });
  lognb('found', sol)
  return sol[0][0];
}

// lognb(_solveMachineB_failedAStar(parse('[###.....] (1,2,3,4,6) (3,6) (1,3,4,6,7) (0,3,5,6,7) (0,1,2) (1,2,4,5,6,7) (0,1,2,4,5,6) {51,48,43,156,33,40,180,25}')[0]))

// %%

/// ATTEMPT 2 -----------------------------------------------------------------------------
/// and here i've finally realized that this probably is some system of equations,
/// so i've tried some gaussian reduction, untill I've realized there are too many vars to do that...
/// ... and thats when i've spent a lot of time on reading about solving linear programming and min/max simplex method...
/// ... but it was too much to implement it, thus finally importing highs for it...

const sorter = (a: number[], b: number[]): number => {
  let dif = 0;
  for (let i = 0; i < a.length; ++i) {
    const bv = Math.abs(b[i]);
    const av = Math.abs(a[i]);
    dif = (bv === 1 ? 100 : bv) - (av === 1 ? 100 : av);
    if (dif !== 0) return dif;
  }
  return dif;
}

const addRowToRow = (g: Grid<number>, from: number, to: number, scalar = 1): Grid<number> => {
  for (let x = 0; x < g.cols; ++x) {
    g.data[to][x] += scalar * g.data[from][x];
  }
  return g;
}
const multRow = (g: Grid<number>, row: number, scalar = 1): Grid<number> => {
  for (let x = 0; x < g.cols; ++x) {
    g.data[row][x] *= scalar;
  }
  return g;
}

const logMat = (mat: Grid<number>) => log(mat.pprint((v) => v!.toString().padStart(5, ' ')));

parse(`
[###.....] (1,2,3,4,6) (3,6) (1,3,4,6,7) (0,3,5,6,7) (0,1,2) (1,2,4,5,6,7) (0,1,2,4,5,6) {51,48,43,156,33,40,180,25}
[#.....##..] (1,4,5,6,7,8) (0,7) (0,1,2,3,5,6,7) (0,1,2,3,4,6,7) (1,2,4,7,8) (0,1,5,6) (6,8,9) (4,6,9) (1,2,3,4,5,6,7,8) (0,4,7,8) (0,3,5,7,8,9) (1,2,3,4,9) (0,1,2,3,5,7,8) {115,123,88,96,101,102,112,144,110,54}
[##..] (2,3) (1,2) (1,2,3) (0,2,3) (0,1) (0) {16,19,21,19}
`).slice(0).forEach(m => {
  lognb(m.target);
  // log(m.joltage.join(','));
  // const names = 'abcdefghijklmnopqrs';
  // log('');
  // log(m.buttons.map((b, i) => names[i] + ':' + b.join(',')).join('\n'))
  // log('');
  // const joltBtns: string[][] = m.joltage.map(_ => []);
  // m.buttons.forEach((b, bi) => b.forEach(i => joltBtns[i].push(names[bi])))
  // log(joltBtns.map((jb, i) => jb.join('+') + ' = ' + m.joltage[i]).join('\n'))
  // log('');
  const mat: Grid<number> = Grid.create(m.buttons.length + 1, m.joltage.length, 0);
  m.buttons.forEach((bn, bi) => bn.forEach(ji => mat.set([bi, ji], 1)));
  m.joltage.forEach((jv, ji) => mat.set([mat.cols - 1, ji], jv));
  logMat(mat)
  mat.data.sort(sorter);
  logMat(mat)

  let curRow = 0;
  for (let col = 0; col < mat.cols - 1; ++col) {
    if (mat.data[curRow][col] === 0) continue;
    if (mat.get(col, curRow) !== 1) {
      multRow(mat, curRow, 1 / mat.get(col, curRow)!);
    }
    if (curRow === mat.rows - 1) {
      if (col < mat.cols - 2) {
        logMat(mat)
        log('too many vars :/');
        return;
      }
      break;
    };
    for (let row = curRow + 1; row < mat.rows; ++row) {
      // y/x
      const scalar = mat.get(col, row)! / mat.get(col, curRow)!;
      addRowToRow(mat, curRow, row, -scalar);
    }
    mat.data.sort(sorter);
    curRow += 1;
    logMat(mat)
  }
  logMat(mat)

  for (let row = mat.rows - 1; row >= 1; --row) {
    const col = mat.data[row].indexOf(1);
    if (col < 0) continue;
    for (let tRow = 0; tRow < row; ++tRow) {
      if (mat.data[tRow][col] !== 0) {
        addRowToRow(mat, row, tRow, -1 / mat.data[tRow][col]);
      }
    }
    logMat(mat)
  }
})

// %%

// const solveB = (pd: ParsedData) => pd.map(solveMachineB).sum();

// lognb(solveB(parse('[##..] (2,3) (1,2) (1,2,3) (0,2,3) (0,1) (0) {16,19,21,19}')))

// %%
