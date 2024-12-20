// %%
import { assertEquals } from "@std/assert";
import { Grid, Vector, aStar, log, isVerbose, $hash } from "#lib";

// %%

const sample = `
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
`
const sampleGridSize = 7;

const parse = (s: string) => s.trim().split('\n').map(Vector.from)
parse(sample)

// %%

const solveA = (vs: Vector[], d: number, n: number, verbose = false): number => {
  const g = Grid.create(d, d, '.');
  for (let i = 0; i < n; ++i) {
    g.set(vs[i], '#');
  }
  const start = new Vector(0, 0);
  const end = new Vector(d - 1, d - 1);

  const resp = aStar({
    start: start,
    isEnd: (v: Vector) => end.eq(v),
    nbs: (v: Vector) => v.orthogonals().filter(nb => g.get(nb) === '.'),
    hash: (v: Vector) => v.toString(),
    score: (v: Vector) => end.manhattan(v),
  });
  if (resp.length === 0) return -1;
  const [[steps, path]] = resp;
  if (verbose) {
    for (const p of path) {
      g.set(p, 'O');
    }
    log(g.pprint())
  }
  return steps;
}
assertEquals(solveA(parse(sample), sampleGridSize, 12, true), 22);

// %%
const data = await Deno.readTextFile('./data/day18.txt');
const dataGridSize = 71;

console.log('Sol A:', solveA(parse(data), dataGridSize, 1024, isVerbose));

// %%

const solveB = (s: string, d: number) => {
  const vs = parse(s);

  const nRange = [0, vs.length];
  while (nRange[0] !== nRange[1]) {
    const toCheck = nRange[0] + Math.floor((nRange[1] - nRange[0]) / 2);
    const hasPath = solveA(vs, d, toCheck);
    if (hasPath > 0) {
      nRange[0] = toCheck + 1;
    } else {
      nRange[1] = toCheck;
    }
  }
  const firstBlockingIdx = nRange[0] - 1;
  return vs[firstBlockingIdx][$hash]();
}
assertEquals(solveB(sample, sampleGridSize), "6,1")
// %%

console.log("Sol B:", solveB(data, dataGridSize))
