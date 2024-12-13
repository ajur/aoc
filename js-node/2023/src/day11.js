
import {grid as G, arrays as A, maths as M} from './utils.js'

export const sample = `
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
`

export const runA = (data) => {
  const grid = parse(data);

  const galaxies = G.findAll(grid, c => c === '#');
  const empty = findEmptyRowsAndCols(galaxies, grid.length, grid[0].length);
  
  return A.pairs(galaxies)
    .map(([[g1r, g1c], [g2r, g2c]]) => 
      Math.abs(g2r - g1r) + empty.rows.filter(M.inRange(g1r, g2r)).length +
      Math.abs(g2c - g1c) + empty.cols.filter(M.inRange(g1c, g2c)).length
    )
    .sum();
}

export const runB = (data) => {
  const grid = parse(data);

  const galaxies = G.findAll(grid, c => c === '#');
  const empty = findEmptyRowsAndCols(galaxies, grid.length, grid[0].length);
  
  return A.pairs(galaxies)
    .map(([[g1r, g1c], [g2r, g2c]]) => 
      Math.abs(g2r - g1r) + empty.rows.filter(M.inRange(g1r, g2r)).length * 999_999 +
      Math.abs(g2c - g1c) + empty.cols.filter(M.inRange(g1c, g2c)).length * 999_999
    )
    .sum();
}

const findEmptyRowsAndCols = (galaxies, nRows, nCols) => {
  const rows = Array.from({length: nRows}, (_, i) => i);
  const cols = Array.from({length: nCols}, (_, i) => i);
  galaxies.forEach(([r, c]) => {
    rows[r] = -1;
    cols[c] = -1;
  });
  return {
    rows: rows.filter(v => v >= 0),
    cols: cols.filter(v => v >= 0)
  };
}

const parse = (str) => str.split('\n')
