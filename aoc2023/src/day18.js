import {Array2D as A2D} from 'array2d'
import {strings as S, arrays as A, functions as F, maths as M, grid as G} from './utils.js'
import {saveGridAsImage} from './img.js'

export const sample = `
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
`

export const runA = data => parseA(data)
  .peek(pathToImage)
  .apply(stepsToPoints)
  .apply(s => M.area(s, true));

export const runB = data => parseB(data)
  .apply(stepsToPoints)
  .apply(s => M.area(s, true));

const stepsToPoints = steps => steps.reduce((points, {dir, dist}) => {
  const [dr, dc] = dirToSteps(dir, dist);
  const [r, c] = points.at(-1);
  points.push([r + dr, c + dc]);
  return points;
}, [[0,0]]);

const dirToSteps = (dir, dist) => {
  switch(dir) {
    case 'L': return [0, -dist];
    case 'R': return [0, dist];
    case 'U': return [-dist, 0];
    case 'D': return [dist, 0];
  }
};

const parseA = (str) => str.split('\n')
  .map(S.split(' '))
  .map(([dir, dist, clr]) => ({
    dir,
    dist: S.asInt(dist),
    clr: clr.substring(1, clr.length -  1)
  }));

const parseB = (str) => str.split('\n')
  .map(line => line.match(/\(#([a-f\d]{5})([a-f\d])\)/))
  .map(([_, distHex, dirNum]) => ({
    dir: 'RDLU'[dirNum],
    dist: parseInt(distHex, 16)
  }))


const pathToImage = (steps) => {
  const points = steps.reduce((path, {dir, dist, clr}) => path.concat(pathByDir(dir, dist).map(([dr,dc]) => [dr + path.at(-1)[0], dc + path.at(-1)[1], clr])), [[0, 0, '#ffffff']])

  const rowsIdxs = points.map(F.fst);
  const rMin = Math.min(...rowsIdxs);
  const rMax = Math.max(...rowsIdxs);
  const colsIdxs = points.map(F.snd);
  const cMin = Math.min(...colsIdxs);
  const cMax = Math.max(...colsIdxs);

  const height = rMax - rMin + 1 + 2;
  const width = cMax - cMin + 1 + 2;
  
  const grid = A2D.build(width, height, '#000000');
  points.forEach(([r, c, clr]) => grid[r - rMin + 1][c - cMin + 1] = clr);

  saveGridAsImage(grid, 'day18out', hexToARGB);
}

const hexToARGB = hex => [
  255,
  parseInt(hex.substring(1,3), 16),
  parseInt(hex.substring(3,5), 16),
  parseInt(hex.substring(5,7), 16)
];

const pathByDir = (dir, dist) => A.range(dist).map(i => {
  switch(dir) {
    case 'L': return [0, -i-1];
    case 'R': return [0, i+1];
    case 'U': return [-i-1, 0];
    case 'D': return [i+1, 0];
  }
})