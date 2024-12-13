

import {grid as G, strings as S, arrays as A} from './utils.js'

export const sample = `
.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
`

export const runA = (data) => parse(data)
  // .peek(d => console.log(G.pprint()(d)))
  .apply(beamLight([0, 0], '→'))

export const runB = (data) => {
  const grid = parse(data);
  const rows = grid.length;
  const cols = grid[0].length;
  const startPoints = [
    ...A.range(rows).flatMap(row => [[[row, 0], '→'], [[row, cols - 1], '←']]),
    ...A.range(cols).flatMap(col => [[[0, col], '↓'], [[rows - 1, col], '↑']])
  ];
  // startPoints.log();
  
  return Math.max(...startPoints.map(([start, dir]) => beamLight(start, dir)(grid)));
}

const beamLight = (start, dir) => (grid) => {
  const next = [[start, dir]];
  const pathMap = G.copy(grid);

  while (next.length > 0) {
    const [[row, col], dir] = next.shift();
    const tile = grid[row]?.[col];
    if (!tile) continue;

    const nextDir = tileToDir[tile][dir];

    if (pathMap[row][col] === nextDir) continue;

    pathMap[row][col] = nextDir;

    switch(nextDir) {
     case '←': next.push([[row, col - 1], nextDir]); break;
     case '↑': next.push([[row - 1, col], nextDir]); break;
     case '→': next.push([[row, col + 1], nextDir]); break;
     case '↓': next.push([[row + 1, col], nextDir]); break;
     case '↔': next.push([[row, col - 1], '←'], [[row, col + 1], '→']); break;
     case '↕': next.push([[row - 1, col], '↑'], [[row + 1, col], '↓']); break;
    }
  }

  // console.log('\n'+G.pprint(v => ' '+v+' ')(pathMap))

  // console.log('\n'+G.pprint(v => '.|-\\/'.indexOf(v) >=0 ? '.' : '#')(pathMap))

  return G.findAll(pathMap, v => ' ←↑→↓↔↕'.indexOf(v) > 0).length;
}
/* ← ↑ → ↓ ↔ ↕ */
const tileToDir = {
  '.': {'←': '←', '↑': '↑', '→': '→', '↓': '↓'},
  '|': {'←': '↕', '↑': '↑', '→': '↕', '↓': '↓'},
  '-': {'←': '←', '↑': '↔', '→': '→', '↓': '↔'},
  '/': {'←': '↓', '↑': '→', '→': '↑', '↓': '←'},
  '\\': {'←': '↑', '↑': '←', '→': '↓', '↓': '→'}
};

const stepByDir = {
  T: ([row, col]) => [row + 1, col],
  B: ([row, col]) => [row - 1, col],
  L: ([row, col]) => [row, col + 1],
  R: ([row, col]) => [row, col - 1]
}

const parse = (str) => str.split('\n')
  .map(S.split(''))
