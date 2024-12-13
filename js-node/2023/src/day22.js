
import {grid as G, strings as S, arrays as A, functions as F} from './utils.js'

export const sample = `
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
`

export const runA = (data) => parse(data)
  .sort(F.asc(b => b.pos[2]))
  .apply(fallingBrics => fallingBrics.reduce(fallBrick, {grid: createGrid(fallingBrics), bricks: []}))
  // .peek(({grid}) => {pprintByXZ(grid).log(); pprintByYZ(grid).log()})
  .apply(({grid, bricks}) => bricks.filter(canBeRemoved(grid)))
  .length;

export const runB = (data) => parse(data)
  .sort(F.asc(b => b.pos[2]))
  .apply(fallingBrics => fallingBrics.reduce(fallBrick, {grid: createGrid(fallingBrics), bricks: []}))
  // .peek(({grid}) => {pprintByXZ(grid).log(); pprintByYZ(grid).log()})
  .apply(({grid, bricks}) => bricks
    .filter(cannotBeRemoved(grid))
    .map(countFallen(grid, bricks))
    .sum());

const countFallen = (oridgGrid, origBricks) => (brick) => {
  const  grid = copyGrid(oridgGrid);
  const bricks = origBricks.map(copyBrick);
  remove(grid, brick);
  // console.log('\n\n> before fall', ".ABCDEFGH"[brick.id])
  // logGrid(grid);
  const checked = {};
  let fallCount = 0;
  for (let z = brick.pos[2]; z < grid[0][0].length; ++z) {
    for (let nextToCheck of bricksAtZ(grid, bricks, z)) {
      if (checked[nextToCheck.id]) continue;
      checked[nextToCheck.id] = true;
      if (canFall(grid, nextToCheck)) {
        ++fallCount;
        remove(grid, nextToCheck)
        fallBrick({grid, bricks}, nextToCheck);
      }
    }
  }
  // console.log('\n< after fall')
  // logGrid(grid);
  return fallCount;
}

const bricksAtZ = (grid, bricks, z) => {
  const found = [];
  for (let x = 0; x < grid.length; ++x) {
    for (let y = 0; y < grid[0].length; ++y) {
      if (grid[x][y][z]) found.push(grid[x][y][z]);
    }
  }
  return found.unique()
    .map(bid => bricks.find(b => b.id === bid));
};

const cannotBeRemoved = (grid) => (...args) => !canBeRemoved(grid)(...args);

const canBeRemoved = (grid) => (brick, i, bricks) => {
  const supported = supports(grid, bricks, brick);
  if (supported.length === 0) return true;
  remove(grid, brick);
  const wouldFall = supported.filter(sp => canFall(grid, sp));
  settle(grid, brick);
  return wouldFall.length === 0;
}

const createGrid = (fallingBrics) => {
  const maxPos = [0, 0, 0];
  fallingBrics.forEach(({pos, blocks}) => {
    const mpos = addPos(pos, blocks.at(-1));
    for (let i = 0; i < 3; ++i) 
      if (mpos[i] > maxPos[i])
        maxPos[i] = mpos[i];
  });
  return Array.from(
    {length: maxPos[0] + 1}, () => Array.from(
      {length: maxPos[1] + 1}, () => Array.from(
        {length: maxPos[2] + 1}, () => 0)));
}

const copyGrid = (grid) => grid.map(xs => xs.map(ys => [...ys]));
const copyBrick = ({id, pos, blocks}) => ({id, pos: [...pos], blocks: blocks.map(b => [...b])});

const fallBrick = ({grid, bricks}, falling) => {
  while(canFall(grid, falling)) {
    falling.pos[2] -= 1;
  }
  settle(grid, falling);
  if (!bricks.find(b => b.id === falling.id)){
    bricks.push(falling);
  }
  return {grid, bricks};
}

const canFall = (grid, brick) => {
  if (brick.pos[2] === 1) {
    return false;
  }
  const newPos = addPos(brick.pos, [0, 0, -1]);
  for (let block of blocksAbs(brick, newPos)) {
    const checkedPos = atPos(grid, block);
    if (checkedPos && checkedPos !== brick.id) {
      return false;
    }
  }
  return true;
}

const settle = (grid, brick) => {
  for (let block of blocksAbs(brick)) {
    setPos(grid, block, brick.id);
  }
}
const remove = (grid, brick) => {
  for (let block of blocksAbs(brick)) {
    setPos(grid, block, 0);
  }
}
const supports = (grid, bricks, brick) => {
  const above = addPos(brick.pos, [0, 0, 1])
  const supports = [];
  for (let block of blocksAbs(brick, above)) {
    const val = atPos(grid, block);
    if (val > 0 && val !== brick.id) {
      supports.push(val);
    }
  }
  return supports.unique()
    .map(bid => bricks.find(b => b.id === bid));
}

const atPos = (grid, [x, y, z]) => grid[x][y][z];
const setPos = (grid, [x, y, z], val) => grid[x][y][z] = val;
const addPos = ([x1,y1,z1], [x2,y2,z2]) => [x1+x2, y1+y2, z1+z2];
const blocksAbs = (brick, start = brick.pos) => brick.blocks.map(block => addPos(start, block));

const parse = (str) => str.split('\n')
  .map(r => r.split('~').map(c => c.split(',').map(S.asInt)))
  .map(([start, end], i) => ({
    id: i + 1,
    pos: start,
    blocks: toBlocks(start, end)
  }))

const toBlocks = ([x0, y0, z0], [x1, y1, z1]) => {
  const blocks = []
  for (let x = x0; x <= x1; ++x) {
    for (let y = y0; y <= y1; ++y) {
      for (let z = z0; z <= z1; ++z) {
        blocks.push([x - x0, y - y0, z - z0])
      }
    }
  }
  return blocks;
}


const logGrid = (grid) => {
  console.log(' xz    yz');
  pprintByXZ(grid).split('\n')
    .zip(pprintByYZ(grid).split('\n'))
    .map(S.join('   '))
    .join('\n').log();
}

const pprintByXZ = (grid) => {
  const out = [];
  for (let z = grid[0][0].length - 1; z > 0; --z) {
    const row = Array.from({length: grid.length}, () => '.');
    for (let x = 0; x < grid.length; ++x) {
      let ys = [];
      for (let y = 0; y < grid[0].length; ++y) {
        if (grid[x][y][z]) ys.push(grid[x][y][z]);
      }
      ys = ys.unique();
      if (ys.length === 1) row[x] = ".ABCDEFGH"[ys[0]] || '#';
      if (ys.length > 1) row[x] = '?';
    }
    out.push(row.join(''));
  }
  out.push(''.padEnd(grid.length, '-'));
  return out.join('\n');
}

const pprintByYZ = (grid) => {
  const out = [];
  for (let z = grid[0][0].length - 1; z > 0; --z) {
    const row = Array.from({length: grid[0].length}, () => '.');
    for (let y = 0; y < grid[0].length; ++y) {
      let xs = [];
      for (let x = 0; x < grid.length; ++x) {
        if (grid[x][y][z]) xs.push(grid[x][y][z]);
      }
      xs = xs.unique();
      if (xs.length === 1) row[y] = ".ABCDEFGH"[xs[0]] || '#';
      if (xs.length > 1) row[y] = '?';
    }
    out.push(row.join(''));
  }
  out.push(''.padEnd(grid[0].length, '-'));
  return out.join('\n');
}
