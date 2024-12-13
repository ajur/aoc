import {default as FastPriorityQueue} from 'fastpriorityqueue'
import { saveGridAsImage } from './img.js';
import {grid as G, strings as S, arrays as A, functions as F, maths as M} from './utils.js'

export const sample = `
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
`

export const runA = (data) => parse(data)
  .apply(solveAndPrint('a'))

export const runB = (data) => parse(data.replaceAll(/[><^v]/g, '.'))
  .apply(solveAndPrint('b'))


const solveAndPrint = (part) => (hikeMap) => {
  const {nodes, edges, start, end} = buildGraph(hikeMap);
  const isSample = (hikeMap.length < 30);

  for (const node of Object.values(nodes)) {
    hikeMap[node.row][node.col] = 'n';
  }    
  mapToImg(hikeMap, [], "day23_" + part + (isSample ? '_s' : '') + '_in');

  const {len, steps} = findLongestPath(edges, start, end);
  
  mapToImg(hikeMap, steps, "day23_" + part + (isSample ? '_s' : '') + '_out');

  return len;
}

const buildGraph = (hmap) => {
  const startNode = createNode({pos: [0, 1]});
  const endNode = createNode({pos: [hmap.length - 1, hmap[0].length - 2]});
  const nodes = {[startNode.key]: startNode, [endNode.key]: endNode};
  const edges = {};

  const pathsToCheck = [{node: startNode, pos: createDirection(1, 0).stepFrom(startNode.pos), dir: createDirection(1, 0)}];

  while (pathsToCheck.length > 0) {
    const pathStart = pathsToCheck.pop();
    const steps = [];
    let current = pathStart;
    let directional = false;
    while (current) {
      steps.push(current.pos);
      if ('<>^v'.indexOf(hmap[current.pos[0]][current.pos[1]]) >= 0) {
        directional = true;
      }
      const next = [current.dir.fw(), current.dir.left(), current.dir.right()]
        .map(dir => ({dir, pos: dir.stepFrom(current.pos)}))
        .filter(({pos}) => '.<>^v'.indexOf(hmap[pos[0]]?.[pos[1]]) >= 0);
      if (next.length === 1) {
        current = next[0];
      } else {
        const node = createNode({pos: current.pos});
        const startKey = pathStart.node.key;
        edges[startKey] ??= {};
        edges[startKey][node.key] = {steps, len: steps.length};
        if (!directional) {
          edges[node.key] ??= {};
          edges[node.key][startKey] = {steps, len: steps.length};
        }
        if (!nodes[node.key]) {
          nodes[node.key] = node;
          next.filter(({pos, dir}) => {
              const type = hmap[pos[0]][pos[1]];
              if (type === '.') {
                return true;
              }
              const nDir = slopeDirection(type);
              return !veq(dir.stepFrom(nDir), [0,0]);
            })
            .forEach(n => {
              pathsToCheck.push({...n, node});
            });
        }
        current = null;
      }
    }
  }

  return {
    nodes,
    edges,
    start: startNode.key,
    end: endNode.key
  }
}

const findLongestPath = (edges, start, end) => {

  const findLongest = (from, visited) => {
    const newVisited = {...visited, [from]: true}
    if (from === '11,21') debugger
    if (from === end) {
      return [0, []];
    }
    if (!edges[from]) {
      return [-1, []];
    }
    const out = Object.entries(edges[from])
      .filter(([next]) => !visited[next])
      .map(([next, steps]) => {
        const [len, path] = findLongest(next, newVisited);
        return [len < 0 ? len : len + steps.len, [steps, ...path]];
      })
      .sort(F.desc(F.fst))
    if (out.length > 0) {
      return out.at(0);
    }
    return [-1, []];
  };

  const [len, path] = findLongest(start, {});
  return {
    len,
    steps: path.flatMap(F.key('steps'))
  };
}


const createNode = (props) => ({
  ...props,
  get row() { return this.pos[0] },
  get col() { return this.pos[1] },
  get key() { return ''+this.row+','+this.col},
  eq: (node) => veq(node.pos, props.pos)
})

const createDirection = (dr, dc) => ({
  [0]: dr,
  [1]: dc,
  row: dr,
  col: dc,
  fw: () => createDirection(dr, dc),
  left: () => createDirection(-dc, dr),
  right: () => createDirection(dc, -dr),
  stepFrom: (rc) => [rc[0] + dr, rc[1] + dc]
})

const slopeDirection = (slope) => {
  switch(slope) {
    case '<': return createDirection(0, -1);
    case '>': return createDirection(0, 1);
    case '^': return createDirection(-1, 0);
    case 'v': return createDirection(1, 0);
  }
}

const veq = (v1, v2) => v1?.[0] === v2?.[0] && v1?.[1] === v2?.[1];


const parse = (str) => str.split('\n')
  .map(r => r.split(''))





const mapToClr = {
  '#': 0x283618,
  '.': 0xdda15e,
  's': 0xbc6c25,
  '*': 0xfefae0,
  'n': 0x145CA9,
}

const tileStrToColors = str => str.join('').trim().split('\n').map(r => r.split('').map(c => mapToClr[c]));

const scale = 5;
const slopeRightTile = tileStrToColors`
sss..
.sss.
..sss
.sss.
sss..`
const nodeTile = tileStrToColors`
..n..
.n.n.
n...n
.n.n.
..n..`


const mapToTile = {
  '#': G.create(5, 5, mapToClr['#']),
  '.': G.create(5, 5, mapToClr['.']),
  '>': slopeRightTile,
  '<': slopeRightTile.map(r => r.toReversed()),
  'v': G.transpose(slopeRightTile),
  '^': G.transpose(slopeRightTile.map(r => r.toReversed())),
  'n': nodeTile
}

const mapToImg = (map, steps, fileName = 'day23out') => {
  const grid = G.flatMap(map, (v, r, c) => mapToTile[v]);
  steps.forEach(([r,c]) => {
    grid[r * scale + 2][c * scale + 2] = mapToClr['*'];
    grid[r * scale + 1][c * scale + 2] = mapToClr['*'];
    grid[r * scale + 3][c * scale + 2] = mapToClr['*'];
    grid[r * scale + 2][c * scale + 1] = mapToClr['*'];
    grid[r * scale + 2][c * scale + 3] = mapToClr['*'];
  });
  saveGridAsImage(grid, fileName)
}
