

import {grid as G, strings as S} from './utils.js'

export const sample = `
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
`

export const runA = (data) => parse(data)
  .map(findMirrored)
  .sum()

export const runB = (data) => parse(data)
  .map(findSemiMirrored)
  .sum()


const findSemiMirrored = (grid) => {
  let row = findSmiMirroredLine(grid);
  let col = -1;
  if (row < 0) {
    const tgrid = G.transpose(grid).map(row => row.join(''));
    col = findSmiMirroredLine(tgrid);
  }
  return (col + 1) + 100 * (row + 1);
}

const findSmiMirroredLine = (grid) => {
  const width = grid[0].length;
  for (let r = 0; r < grid.length - 1; ++r) {
    let diffCount = 0;
    for (let i = 0; i < width; ++i) {
      if (grid[r][i] !== grid[r + 1][i]) {
        diffCount += 1;
        if (diffCount > 1) break;
      }
    }
    if (diffCount < 2) {
      diffCount = 0;
      for (let r1 = r, r2 = r + 1; r1 >= 0 && r2 < grid.length; --r1, ++r2) {
        for (let i = 0; i < width; ++i) {
          if (grid[r1][i] !== grid[r2][i]) {
            diffCount += 1;
            if (diffCount > 1) break;
          }
        }
        if (diffCount > 1) break;
      }
      if (diffCount === 1) return r;
    }
  }
  return -1;
}


const findMirrored = (grid) => {
  let row = findMirroredLine(grid);
  let col = -1;
  if (row < 0) {
    const tgrid = G.transpose(grid).map(row => row.join(''));
    col = findMirroredLine(tgrid);
  }
  return (col + 1) + 100 * (row + 1);
}

const findMirroredLine = (grid) => {
  let mirrorIdx = -1;
  for (let r = 0; r < grid.length - 1; ++r) {
    if (grid[r] === grid[r + 1]) {
      if (verifyMirror(grid, r)) {
        mirrorIdx = r;
      }
    }
  }
  return mirrorIdx;
}

const verifyMirror = (grid, row) => {
  for (let r1 = row, r2 = row + 1; r1 >= 0 && r2 < grid.length; --r1, ++r2) {
    if (grid[r1] !== grid[r2]) {
      return false;
    }
  }
  return true;
}

const parse = (str) => str.split('\n\n')
  .map(S.split('\n'))