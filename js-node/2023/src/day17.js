import {default as FastPriorityQueue} from 'fastpriorityqueue'
import { saveGridAsImage } from './img.js';
import {grid as G, strings as S, arrays as A, functions as F, maths as M} from './utils.js'

export const sample = `
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
`

const sol = (partName, min, max) => (data) => parse(data)
  .peek(hmap => mapToImg(hmap, `day17_${partName}${hmap.length < 15 ? '_s' : ''}_input`))
  .apply(hmap => {
    const path = findPath(hmap, [0,0], [hmap.length - 1, hmap[0].length - 1], min, max);
    path.forEach(({row, col, dir}) => {hmap[row][col] = [hmap[row][col], dir.key]});
    mapToImg(hmap, `day17_${partName}${hmap.length < 15 ? '_s' : ''}_path`)
    
    return path.at(-1).g;
  });

export const runA = sol('a', 1, 3);

export const runB = sol('b', 4, 10);

const findPath = (hmap, start, end, minSteps, maxSteps) => {
  const closed = {};
  const open = new FastPriorityQueue((a, b) => a.f < b.f);
  open.add(createNode({pos: start, dir: direction(1, 0), dist: 1, g: 0, h: M.manhatan(start, end)}));
  open.add(createNode({pos: start, dir: direction(0, 1), dist: 1, g: 0, h: M.manhatan(start, end)}));

  const getNext = nextStepGetter(hmap, end, minSteps, maxSteps)

  while (!open.isEmpty()) {
    const q = open.poll();

    // process.stdout.write('\r'+q.h)

    if (q.h === 0) {
      let curr = q;
      const path = [];
      while (curr) {
        path.push(curr);
        curr = curr.parent;
      }
      return path.reverse();
    }

    if (closed[q.key]) continue;

    const nbs = getNext(q);

    for (let nb of nbs) {
      open.add(nb);
    }

    closed[q.key] = q;
  }
  return null;
}

const nextStepGetter = (hmap, endPos, min, max) => {
  const step = (node, dir) => {
    const npos = dir.add(node.pos);
    const [row, col] = npos;
    if (!hmap[row]?.[col]) return null;
    let p = node;
    return createNode({
      pos: npos,
      dir,
      g: node.g + hmap[row][col],
      h: M.manhatan(npos, endPos),
      parent: node
    });
  }
  const walk = (node, dir) => {
    const steps = [];
    let i = 1;
    let prev = node;
    for (; i < min; ++i) {
      prev = step(prev, dir);
      if (!prev) return steps;
      if (prev.h === 0) steps.push(prev);
    }
    for (; i <= max; ++i) {
      prev = step(prev, dir);
      if (!prev) return steps;
      steps.push(prev);
    }
    return steps;
  }
  
  return (node) => ([
    ...walk(node, node.dir.left()),
    ...walk(node, node.dir.right())
  ]);
}


const createNode = (props) => ({
  ...props,
  get row() { return this.pos[0] },
  get col() { return this.pos[1] },
  get f() { return this.g },
  get key() { return ''+this.row+','+this.col+'d'+this.dir.vh},
  eq: (node) => veq(node.pos, props.pos) && node.dir.vh === props.dir.vh
})

const direction = (dr, dc) => ({
  [0]: dr,
  [1]: dc,
  row: dr,
  col: dc,
  fw: () => direction(dr, dc),
  left: () => direction(-dc, dr),
  right: () => direction(dc, -dr),
  add: ([r, c]) => [r + dr, c + dc],
  get key() { return ''+this.row+','+this.col},
  get vh() { return dr === 0 ? 'h' : 'v'}
})

const veq = (v1, v2) => v1[0] === v2[0] && v1[1] === v2[1];

const parse = (str) => str.split('\n')
  .map(r => r.split('').map(S.asInt))

const numToColor = [
  0xff0000,
  0x000000,
  0x1e1e1e,
  0x363636,
  0x4f4f4f,
  0x6a6a6a,
  0x878787,
  0xa4a4a4,
  0xc3c3c3,
  0xe2e2e2
];



const tileStrToColors = str => str.trim().split('\n').map(r => r.split('').map(c => numToColor[S.asInt(c)]));

const scale = 7;
const slopeRightTile = (base) => tileStrToColors(`
0......
000....
.0000..
..00000
.0000..
000....
0......`.replaceAll('.', base))


const pathToTile = (el) => {
  if (Number.isInteger(el)) {
    return G.create(scale, scale, numToColor[el]);
  }
  const [base, dirkey] = el;
  const slopeRight = slopeRightTile(base);
  switch(dirkey) {
    case '0,1': return slopeRight;
    case '0,-1': return slopeRight.map(r => r.toReversed());
    case '1,0': return G.transpose(slopeRight);
    case '-1,0': return G.transpose(slopeRight.map(r => r.toReversed()));
  }
}

const mapToImg = (map, fileName = 'day17out') => {
  const grid = G.flatMap(map, (v, r, c) => pathToTile(v));
  saveGridAsImage(grid, fileName)
}
