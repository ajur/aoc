// %%

import { BinaryHeap } from "@std/data-structures";
import { Grid, VecTuple, Vector, aStar, fst, timeout, display, updateDisplay, log, $hash } from "#lib";


// %%

const isWall = ([x, y]: VecTuple, fav: number): number => {
  let v = x * x + 3 * x + 2 * x * y + y + y * y + fav;
  // count based on Hamming weight wiki page
  let count = 0;
  for (; v; ++count) {
    v &= v - 1;
  }
  return count % 2;
}

const g = new Grid<number>();
for (let y = 0; y < 7; ++y) {
  for (let x = 0; x < 10; ++x) {
    const v = [x, y] as VecTuple;
    g.set(v, isWall(v, 10))
  }
}

log(g.pprint((v) => v ? '#' : '.'))
// %%

const solveA = async (fav: number, start: VecTuple, end: VecTuple) => {
  const grid = new Grid<number>();
  const tileType = (v: VecTuple) => isWall(v, fav) ? 0 : 1;
  grid.set(start, 3);
  grid.set(end, 4);

  const gv2s = ['#', '.', 'O', 'S', 'T'];
  const pg = () => grid.pprint((v) => v === undefined ? ' ' : gv2s[v]);

  display(pg())

  const [[len, path]] = aStar({
    start: new Vector(start),
    isEnd: (v: Vector) => v.eq(end),
    nbs: (v: Vector) => {
      const nbrs = grid.neighbours(v);
      nbrs.forEach((nb) => {
        if (nb[1] === undefined) {
          nb[1] = tileType(nb[0]);
          grid.set(nb[0], nb[1]);
        }
      });
      updateDisplay(pg())
      return nbrs.filter(([_, val]) => val! > 0).map(fst) as Vector[]
    },
    score: (v: Vector) => v.manhattan(end),
    hash: (v: Vector) => v[$hash](),
  });

  for (const v of path) {
    if (!v.eq(start) && !v.eq(end)) grid.set(v, 2);
  }
  await timeout(100);
  await updateDisplay(pg())
  return len;
}
await solveA(10, [1, 1], [7, 4]);

// %%

console.log("Sol A: ", await solveA(1358, [1, 1], [31,39]));

// %%

const solveB = async (fav: number, start: VecTuple, maxDist: number) => {
  const grid = new Grid<number>();
  const tileType = (v: VecTuple) => isWall(v, fav) ? 0 : 1;
  grid.set(start, tileType(start));

  const gv2s = ['#', '.'];
  const pg = () => grid.pprint((v) => v === undefined ? ' ' : gv2s[v]);
  await display(pg())

  const nbs = (v: Vector) => {
    const nbrs = grid.neighbours(v);
    nbrs.forEach((nb) => {
      if (nb[1] === undefined) {
        nb[1] = tileType(nb[0]);
        grid.set(nb[0], nb[1]);
      }
    });
    updateDisplay(pg())
    return nbrs.filter(([_, val]) => val! > 0).map(fst) as Vector[]
  };
  const hash = ([x, y]: VecTuple) => `${x},${y}`;

  const queue = new BinaryHeap<[Vector, number, string, Vector | null]>
  queue.push([new Vector(start), 0, hash(start), null])
  const seen = new Map<string, number>();
  seen.set(hash(start), 0);

  while (queue.length > 0) {
    const [tile, dist, hsh] = queue.pop()!;

    if (dist === maxDist || dist > seen.get(hsh)!) continue;

    for (const nb of nbs(tile)) {
      const nbDist = dist + 1;
      const nbHsh = hash(nb);
      const sn = seen.get(nbHsh) ?? Infinity;
      if (sn > nbDist) {
        queue.push([nb, nbDist, nbHsh, tile]);
        seen.set(nbHsh, nbDist);
      }
    }
    await updateDisplay(pg())
  }

  await timeout(100);
  await updateDisplay(pg())
  return [...seen.values()].filter(v => v <= maxDist).length;
}

console.log("Sol B: ", await solveB(1358, [1, 1], 50));
