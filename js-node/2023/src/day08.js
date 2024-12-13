
import {functions as F, maths as M} from './utils.js'

export const sample = `
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
`

export const sampleB = `
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
`

export const runA = (data) => {
  const {path, map} = parse(data);
  let steps = 0;
  let pathIdx = 0;
  let node = 'AAA';
  while (node !== 'ZZZ') {
    node = map[node][path[pathIdx]];
    ++steps;
    pathIdx = (pathIdx + 1) % path.length;
  }
  return steps;
}

export const runB = (data) => {
  const {path, map} = parse(data);
  const steps = Object.keys(map)
    .filter(s => s.at(-1) === 'A')
    .map(stepsAtoZ(path, map))
    .map(F.key('steps'));
  return M.lcm(...steps);
}

const stepsAtoZ = (path, map) => (start) => {
  let steps = 0;
  let pathIdx = 0;
  let node = start;
  while (node.at(-1) !== 'Z') {
    node = map[node][path[pathIdx]];
    ++steps;
    pathIdx = (pathIdx + 1) % path.length;
  }
  return {start, node, steps};
}



const parse = (str) => {
  const [lr, mapStr] = str.split('\n\n');
  return {
    path: lr.trim(),
    map: Object.fromEntries(mapStr.split('\n').map(row => row.match(/(\w+) = \((\w+), (\w+)\)/)).map(([_, k, l, r]) => [k, {L: l, R: r}]))
  }
}
  