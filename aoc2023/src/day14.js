

import {strings as S, functions as F, grid as G} from './utils.js'

export const sample = `
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
`

export const runA = (data) => parse(data)
  // .peek(d => console.log(d.map(S.join('')).join('\n'), '\n'))
  .apply(tiltNorth)
  // .peek(d => console.log(d.map(S.join('')).join('\n'), '\n'))
  .map((row, idx, grid) => row.count(F.is('O')) * (grid.length - idx))
  .sum()

export const runB = (data) => parse(data)
  .apply(grid => {
    const N = 1000000000;
    const hash = g => g.map(S.join('')).join('\n');
    const cache = {[hash(grid)]: 0};
    const repeated = {};
    const test = (grid, after) => {
      const h = hash(grid);
      if (Object.hasOwn(cache,h)) {
        if (Object.hasOwn(repeated, h)) {
          // console.log('\nREPEATED at', after, 'from', cache[h], '\n', h, '\n');
          // console.log('REPEATED TYPES from', Object.values(repeated))
          // console.log("REPEATED from", repeated[h], 'after', after);
          return repeated[h];
        } else {
          repeated[h] = after;
        }
        return -1;
      } else {
        cache[h] = after;
        return -1;
      }
    }
    let finalRepeated = null;
    for (let i = 0; i < N; ++i) {
      tiltNorth(grid);
      tiltWest(grid);
      tiltSouth(grid);
      tiltEast(grid);
      const repeatedFrom = test(grid, i + 1);
      if (repeatedFrom >= 0) {
        // console.log("REPEATED from", repeatedFrom, 'after', i + 1);
        finalRepeated = repeatedFrom + (N - repeatedFrom) % (i + 1 - repeatedFrom)
        break;
      }
    }
    
    return Object.entries(repeated)
      .find(([g, n]) => n === finalRepeated)
      .apply(F.fst)
      .split('\n')
      .map(S.split(''));
  })
  .map((row, idx, grid) => row.count(F.is('O')) * (grid.length - idx))
  .sum()

const tiltNorth = (grid) => {
  const rows = grid.length;
  const cols = grid[0].length;
  for (let col = 0; col < cols; ++col) {
    let nextFreeSpace = -1;
    for (let row = 0; row < rows; ++row) {
      if (nextFreeSpace < 0 && grid[row][col] === '.') {
        nextFreeSpace = row;
      }
      if (grid[row][col] === 'O' && nextFreeSpace >= 0) {
        grid[nextFreeSpace][col] = 'O';
        grid[row][col] = '.';
        ++nextFreeSpace;
      }
      if (grid[row][col] === '#') {
        nextFreeSpace = -1;
      }
    }
  }
  return grid;
}

const tiltSouth = (grid) => {
  const rows = grid.length;
  const cols = grid[0].length;
  for (let col = 0; col < cols; ++col) {
    let nextFreeSpace = -1;
    for (let row = rows - 1; row >= 0; --row) {
      if (nextFreeSpace < 0 && grid[row][col] === '.') {
        nextFreeSpace = row;
      }
      if (grid[row][col] === 'O' && nextFreeSpace >= 0) {
        grid[nextFreeSpace][col] = 'O';
        grid[row][col] = '.';
        --nextFreeSpace;
      }
      if (grid[row][col] === '#') {
        nextFreeSpace = -1;
      }
    }
  }
  return grid;
}

const tiltWest = (grid) => {
  const rows = grid.length;
  const cols = grid[0].length;
  for (let row = 0; row < rows; ++row) {
    let nextFreeSpace = -1;
    for (let col = 0; col < cols; ++col) {
      if (nextFreeSpace < 0 && grid[row][col] === '.') {
        nextFreeSpace = col;
      }
      if (grid[row][col] === 'O' && nextFreeSpace >= 0) {
        grid[row][nextFreeSpace] = 'O';
        grid[row][col] = '.';
        ++nextFreeSpace;
      }
      if (grid[row][col] === '#') {
        nextFreeSpace = -1;
      }
    }
  }
  return grid;
}

const tiltEast = (grid) => {
  const rows = grid.length;
  const cols = grid[0].length;
  for (let row = 0; row < rows; ++row) {
    let nextFreeSpace = -1;
    for (let col = cols - 1; col >= 0; --col) {
      if (nextFreeSpace < 0 && grid[row][col] === '.') {
        nextFreeSpace = col;
      }
      if (grid[row][col] === 'O' && nextFreeSpace >= 0) {
        grid[row][nextFreeSpace] = 'O';
        grid[row][col] = '.';
        --nextFreeSpace;
      }
      if (grid[row][col] === '#') {
        nextFreeSpace = -1;
      }
    }
  }
  return grid;
}


const parse = (str) => str.split('\n')
  .map(S.split(''))
  