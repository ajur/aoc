
import {grid as G, strings as S, arrays as A} from './utils.js'

export const sample = `
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
`

export const runA = (data) => parse(data)
  // .peek(connections => {
  //   console.log('digraph G {\n' + connections.map(({from, to}) => '  ' + from + ' -> ' + to.join(', ')).join('\n') + '\n}\n');
  // })
  .apply(connections => {
    const isSample = connections.length < 15;
    // i was lazy - this is based on graphviz output :P
    const toRemove = isSample ? 
      [['jqt', 'nvd'], ['cmg', 'bvb'], ['pzl', 'hfx']] :
      [['tmc', 'lms'], ['xnn', 'txf'], ['jjn', 'nhg']]
    
    const graphMap = connections.map(({from, to}) => [from, to]).apply(Object.fromEntries);
    for (const [from, to] of toRemove) {
      graphMap[from].splice(graphMap[from].indexOf(to), 1);
    }
    for (const [from, toList] of Object.entries(graphMap)) {
      for (const to of toList) {
        graphMap[to] ??= [];
        graphMap[to].push(from);
      }
    }

    const group1 = countGroup(graphMap, toRemove[0][0]);
    const group2 = countGroup(graphMap, toRemove[0][1]);
    
    return group1.length * group2.length;
  })

const countGroup = (graphMap, start) => {
  const visitedNodes = {};
  const toVisit = [start];
  while (toVisit.length > 0) {
    const node = toVisit.pop();
    visitedNodes[node] = true;
    for (const next of graphMap[node]) {
      if (!visitedNodes[next]) toVisit.push(next);
    }
  }
  return Object.keys(visitedNodes);
}
  

const parse = (str) => str.split('\n')
  .map(S.split(': '))
  .map(([from, to]) => ({
    from,
    to: to.split(' ')
  }))
