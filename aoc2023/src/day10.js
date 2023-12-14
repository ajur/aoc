
import {Array2D as A2D} from 'array2d';
import {functions as F, strings as S} from './utils.js'

export const sample = `
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
`

export const sampleB = `
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
`

// export const sampleB = `
// FF7FSF7F7F7F7F7F---7
// L|LJ||||||||||||F--J
// FL-7LJLJ||||||LJL-77
// F--JF--7||LJLJ7F7FJ-
// L---JF-JLJ.||-FJLJJ7
// |F|F-JF---7F7-L7L|7|
// |FFJF7L7F-JF7|JL---7
// 7-L-JL7||F7|L7F-7F7|
// L.L7LFJ|||||FJL7||LJ
// L7JLJL-JLJLJL--JLJ.L
// `

export const runA = (data) => {
  const map = parse(data);
  const startPos = A2D.find(map, c => c === 'S')[0];
  // console.log(startPos);

  let paths = Object.entries(stepByDir)
    .filter(([dir, fn]) => {
      const [r, c] = fn(startPos);
      const tile = map[r]?.[c];
      return tileToDir[tile]?.[dir];
    })
    .map(([dir]) => [dir, ...startPos])
    // .log()
  
  if (paths.length !== 2) {
    throw "Only two paths from start expected " + JSON.stringify(paths)
  }

  let steps = 0
  let prevPaths = [['.', -1, -1], ['.', -1, -1]];

  const pathsMet = () => (paths[0][1] === paths[1][1] && paths[0][2] === paths[1][2]);

  do {
    ++steps;
    prevPaths = paths;
    paths = paths.map(([dir, row, col]) => {
      const [nrow, ncol] = stepByDir[dir]([row, col]);
      const tile = map[nrow][ncol];
      return [tileToDir[tile][dir], nrow, ncol];
    })//.log(steps.toString().padStart(3, ' ') + ':');
  } while (!pathsMet());

  return steps;
}

export const runB = (data) => {
  const map = parse(data);
  const startPos = A2D.find(map, c => c === 'S')[0];

  let firstSteps = Object.entries(stepByDir)
    .filter(([dir, fn]) => {
      const [r, c] = fn(startPos);
      const tile = map[r]?.[c];
      return tileToDir[tile]?.[dir];
    })
    .map(([dir]) => [...startPos, dir])
  
  const firstStepsDirs = firstSteps.map(F.nth(2))
  const startTile = Object.entries(tileToDir)
    .filter(([tile, dirs]) => Object.values(dirs).intersection(firstStepsDirs).length === 2)
    .map(F.fst).at(0);
  
  const cleanMap = A2D.build(map[0].length, map.length, '.');
  const path = [{
    pos: startPos,
    tile: startTile
  }];
  cleanMap[startPos[0]][startPos[1]] = startTile;
  
  let step = firstSteps[0];

  while (true) {
    const [row, col, dir] = step;
    const [nrow, ncol] = stepByDir[dir]([row, col]);
    const tile = map[nrow][ncol];
    if (tile === 'S') {
      break;
    }
    path.push({
      pos: [nrow, ncol],
      tile: tile
    });
    cleanMap[nrow][ncol] = tile;
    step = [nrow, ncol, tileToDir[tile][dir]];
  }

  // pprint(cleanMap.map(row => row.join('')).join('\n')).log();

  const expMap = cleanMap
    .flatMap(row => row.map(tile => expanedTile[tile]).reduce((agg, cell) => [[...agg[0], ...cell[0]],[...agg[1], ...cell[1]],[...agg[2], ...cell[2]]]))
  
  expMap.map(r => r.join('')).join('\n').replaceAll('0',' ').log()
    
  return floodMap(expMap)
    .peek(m => m
      .map(r => r.join('')).join('\n').replaceAll('0',' ').replaceAll('5', '~')
      .log()
    )
    .map(row => row.filter(c => c == 1).length)
    .sum();
}

const floodMap = (map) => {
  const toCheck = [[0,0]];
  
  while(toCheck.length > 0) {
    const [row, col] = toCheck.pop();
    if (map[row][col] >= 5) {
      continue;
    }
    map[row][col] = 5;
    const nbs = neighbours(row, col)
      .filter(([r,c]) => map[r]?.[c] < 5);
    toCheck.push(...nbs); 
  }
  return map;
}

const neighbours = (row, col) => [[row - 1, col], [row, col - 1], [row + 1, col], [row, col + 1]];

const expanedTile = {
  '.': [
    [0, 0, 0],
    [0, 1, 0],
    [0, 0, 0],
  ],
  '|': [
    [0, 8, 0],
    [0, 8, 0],
    [0, 8, 0],
  ],
  '-': [
    [0, 0, 0],
    [8, 8, 8],
    [0, 0, 0],
  ],
  'F': [
    [0, 0, 0],
    [0, 8, 8],
    [0, 8, 0],
  ],
  '7': [
    [0, 0, 0],
    [8, 8, 0],
    [0, 8, 0],
  ],
  'L': [
    [0, 8, 0],
    [0, 8, 8],
    [0, 0, 0],
  ],
  'J': [
    [0, 8, 0],
    [8, 8, 0],
    [0, 0, 0],
  ]
};

const tileToDir = {
  '|': {T: 'T', B: 'B'},
  '-': {L: 'L', R: 'R'},
  'F': {B: 'L', R: 'T'},
  '7': {L: 'T', B: 'R'},
  'L': {T: 'L', R: 'B'},
  'J': {L: 'B', T: 'R'}
};

const stepByDir = {
  T: ([row, col]) => [row + 1, col],
  B: ([row, col]) => [row - 1, col],
  L: ([row, col]) => [row, col + 1],
  R: ([row, col]) => [row, col - 1]
}

const pprint = (data) => {
  const remap = {
    '.': ' ',
    '|': '║',
    '-': '═',
    'F': '╔',
    '7': '╗',
    'L': '╚',
    'J': '╝',
    '\n': '\n',
    'S': 'S'
  };
  const out = Array.from({length: data.length});
  for (let i = 0; i < data.length; ++i) {
    out[i] = remap[data[i]];
  }
  return out.join('');
}
  

const parse = (str) => str.split('\n')
  .map(row => row.split(''));
  