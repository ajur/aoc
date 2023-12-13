
import {Array2D as a2d} from 'array2d';
import './utils.js'

export const sample = `
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
`

export const runA = (data) => {
  const grid = data.split('\n');
  const nums = getAllNums(grid);
  
  return nums.filter(num => {
    const subgrid = a2d.crop(grid, num.row - 1, num.col - 1, num.length + 2, 3);
    const symbols = a2d.find(subgrid, c => c.match(/[^.\d]/));
    return symbols.length > 0;
  }).map(num => num.val).sum();
};

export const runB = (data) => {
  const grid = data.split('\n');
  const nums = getAllNums(grid);
  
  const gearsWithNums = nums
    .map(num => {
      const subgrid = a2d.harvest(grid, num.row - 1, num.col - 1, num.length + 2, 3);
      const gears = a2d.find(subgrid, c => c?.match(/[*]/))?.map(([r,c]) => `${r + num.row - 1},${c + num.col - 1}`);
      return {num: num.val, gears}
    })
    .filter(({num, gears}) => gears.length > 0)
    .reduce((agg, {num, gears}) => {
      for (let g of gears) {
        if (agg[g]) agg[g].push(num);
        else agg[g] = [num];
      }
      return agg;
    }, {});
  // console.log(gearsWithNums);
  const out = Object.values(gearsWithNums)
    .filter(a => a.length === 2)
    .map(([l, r]) => l * r)
    .sum();
  // console.log(out);
  return out;
}

const getAllNums = (grid) => {
  const nums = [];
  for (let row = 0; row < grid.length; ++row) {
    for (let match of grid[row].matchAll(/\d+/g)) {
      nums.push({
        val: parseInt(match[0]),
        row,
        col: match.index,
        length: match[0].length
      });
    }
  }
  return nums;
}