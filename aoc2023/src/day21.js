import {grid as G, strings as S, arrays as A} from './utils.js'
import {saveGridAsImage} from './img.js'

export const sample = `
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
`

export const runA = (data) => {
  let map = parse(data);
  const start = G.find(map, s => s ==='S');
  const N = 64;

  for (let i = 0; i < N; ++i){
    map = markNextSteps(map)
  }
  
  logMap(map, 'day21Sa'+N)

  return G.count(map, c => c === 'O');
}


export const runB = (data) => {
  // this wont work for sample data
  const baseMap = parse(data);
  const mapSize = baseMap.length;
  
  const NN = 26501365;
  const nSizeTimes = Math.floor(NN / mapSize); // 202300
  const nDiff = NN % mapSize; // 65 ~ dist from center to edge

  let map = G.flatMap(G.create(5,5), (v, r, c) => r === 2 && c === 2 ? baseMap : parse(data.replace('S','.')));
  
  // logMap(map, 'out/day21b')
  const N = Math.floor(mapSize / 2) + mapSize * 2;

  for (let i = 0; i < N; ++i){
    map = markNextSteps(map)
    // if (i % mapSize === nDiff - 1) {
    //   console.log('>', (i + 1), '>', G.count(map, c => c === 'O'))
    //   logMap(map, 'day21Sbl'+(i+1))
    // }
  }
  
  // console.log('>', (N), '>', G.count(map, c => c === 'O'))
  // logMap(map, 'day21b' + N)

  const countsByPart = G.map(G.create(5,5,0), (_, row, col) => {
    let count = 0;
    G.forArea(map, row * mapSize, col * mapSize, mapSize, mapSize, (v) => {
      if (v === 'O') ++count;
    });
    return count;
  })

  // console.log(countsByPart.map(row => row.map(v => (''+v).padStart(4,' ')).join(' ')).join('\n'))
  // console.log(namedPartsCounts(countsByPart));
  // console.log(countsByPart.flat().sum())
  // console.log(countsByN(2, namedPartsCounts(countsByPart)))
  return countsByN(nSizeTimes, namedPartsCounts(countsByPart));
}

const namedPartsCounts = (counts) => {
  const namesMap = `
  .. D1 E2 D2 ..
  D1 C1 BB C2 D2
  E1 BB AA BB E3
  D4 C4 BB C3 D3
  .. D4 E4 D3 ..
  `.trim().split('\n').map(row => row.trim().split(' '));
  const namedCounts = {};
  G.forEach(namesMap, (name, row, col) => {
    namedCounts[name] ??= counts[row][col];
  });
  return namedCounts;
}

const countsByN = (N, {AA, BB, C1, C2, C3, C4, D1, D2, D3, D4, E1, E2, E3, E4}) => (
  AA * ((N - 1) ** 2) + BB * (N ** 2) + (C1+C2+C3+C4) * (N - 1) + (D1+D2+D3+D4) * N + E1 + E2 + E3 + E4
);

const markNextSteps = (map) => {
  // this is hightly inefficient, but fuck it, its enough to do 5 * data size, and that is all we need
  const last = [];
  const nextMap = G.map(G.copy(map), (v, row, col) => {
    if (v === 'S' || v === 'O') {
      last.push([row, col])
      return '.'
    }
    return v;
  });
  for (let [row, col] of last) {
    for (let [nr, nc] of G.neighbours(row, col)){
      if (map[nr]?.[nc] && map[nr][nc] !== '#') {
        nextMap[nr][nc] = 'O'
      }
    }
  }
  return nextMap;
}


const colorsMap = {
  '.': [255, 0, 0, 0],
  '#': [255, 0xaf, 0xaf, 0xaf],
  'O': [255, 0, 0, 0xaf],
  'S': [255, 0xff, 0, 0]
}
const logMap = (map, fileName) => {
  const mapStr = map.map(S.join('')).join('\n');
  if (!fileName)
    console.log('\n' + mapStr)
  else
    saveGridAsImage(map, fileName, v => colorsMap[v])
}

const parse = (str) => str.split('\n')
  .map(S.split(''))



/*
Leaving some notes, for proof that even simple math can be hard ;)
                    and that visualizing is the key for me

// > 65 > 3868
// > 196 > 34368     
// > 327 > 95262    
// > 458 > 186550
// > 589 > 308232   309312



nSizeTimes == 2

   0  978 5733  956    0         D1 E2 D2    
 978 6659 7613 6658  956      D1 C1 BB C2 D2     
5720 7613 7584 7613 5732      E1 BB AA BB E3        
 977 6645 7613 6658  940      D4 C4 BB C3 D3 
   0  977 5719  940    0         D4 E4 D3         

1 * 7584 + 4 * 7613 + 1 * 6659 + 1 * 6658 + 1 * 6658 + 1 * 6645 + 2 * 978 + 2 * 956 + 2 * 977 + 2 * 940 + 5720 + 5733 + 5732 + 5719

1 * AA + 4 * BB + 1 * C1 + 1 * C2 + 1 * C3 + 1 * C4 + 2 * D1 + 2 * D2 + 2 * D3 + 2 * D4 + E1 + E2 + E3 + E4



nSizeTimes == 4

   0    0    0  978 5733  956    0    0    0
   0    0  978 6659 7613 6658  956    0    0
   0  978 6659 7613 7584 7613 6658  956    0
 978 6659 7613 7584 7613 7584 7613 6658  956
5720 7613 7584 7613 7584 7613 7584 7613 5732
 977 6645 7613 7584 7613 7584 7613 6658  940
   0  977 6645 7613 7584 7613 6658  940    0
   0    0  977 6645 7613 6658  940    0    0
   0    0    0  977 5719  940    0    0    0


9 * AA + 
16 * BB + 
3 * (C1 + C2 + C3 + C4) + 
4 * (D1 + D2 + D3 + D4) + 
E1 + E2 + E3 + E4



nSizeTimes == 6

E D
B C D
A B C D
B A B C D
A B A B C D
B A B A B C D


(4 * 6 + 1) * A +
4 * 9 * B +
4 * 5 * C +
4 * 6 * D +
E


nSizeTimes == 8

E D
B C D
A B C D
B A B C D
A B A B C D
B A B A B C D
A B A B A B C D
B A B A B A B C D



result: 
N  2  4  6  8

A  1  9 25 49        (N - 1) ** 2
B  4 16 36 64         N ** 2
C4 1  3  5  7         N - 1
D4 2  4  6  8         N
E  1  1  1  1


EQ = AA * (N - 1) ** 2 + BB * N ** 2 + (C1+C2+C3+C4) * (N - 1) + (D1+D2+D3+D4) * N + E1 + E2 + E3 + E4


*/