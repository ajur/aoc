// %%

import { BinaryHeap } from "@std/data-structures";
import { aStar } from "./astar.ts";
import { fst, manhattan, neighbours, timeout, ulog, Vec, Vector, VecTuple, vx, vy } from "./lib.ts";

// %%

class SparseGrid<T> {
  data: T[][];
  width = 0;
  height = 0;

  constructor() {
    this.data = [];
  }

  at(v: Vec): T | undefined {
    const x = vx(v);
    const y = vy(v);

    return this.data[y]?.[x];
  }

  set(v: Vec, val: T) {
    const x = vx(v);
    const y = vy(v);

    this.data[y] ??= [];
    this.data[y][x] = val;

    if (x >= this.width) this.width = x + 1;
    if (y >= this.height) this.height = y + 1;
  }

  neighbours(v: Vec): [Vector, T | undefined][] {
    return neighbours(v)
      .filter((nv) => nv.x >= 0 && nv.y >= 0)
      .map((nv) => [nv, this.at(nv)]);
  }
}

const printGrid = <T>(g: SparseGrid<T>, m: (vt: VecTuple, v: T | undefined) => string) => {
  const out: string[] = [];
  for (let y = 0; y < g.height; ++y) {
    for (let x = 0; x < g.width; ++x) {
      const v = g.at([x, y]);
      out.push(m([x, y], v));
    }
    out.push('\n');
  }
  return out.join('');
}

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

let g = new SparseGrid<number>();
for (let y = 0; y < 7; ++y) {
  for (let x = 0; x < 10; ++x) {
    const v = [x, y] as VecTuple;
    g.set(v, isWall(v, 10))
  }
}

console.log(printGrid(g, (_, v) => v ? '#' : '.'))
// %%

const solveA = async (fav: number, start: VecTuple, end: VecTuple) => {
  const grid = new SparseGrid<number>();
  const tileType = (v: VecTuple) => isWall(v, fav) ? 0 : 1;
  grid.set(start, 3);
  grid.set(end, 4);

  const gv2s = ['#', '.', 'O', 'S', 'T'];
  const pg = () => printGrid(grid, (_, v) => v === undefined ? ' ' : gv2s[v]);

  const drawer = await ulog(`solAout${fav}${start}${end}`, pg())

  const [len, path] = aStar({
    start: new Vector(start),
    end: new Vector(end),
    nbs: (v: Vector) => {
      const nbrs = grid.neighbours(v);
      nbrs.forEach((nb) => {
        if (nb[1] === undefined) {
          nb[1] = tileType(nb[0]);
          grid.set(nb[0], nb[1]);
        }
      });
      drawer(pg())
      return nbrs.filter(([_, val]) => val! > 0).map(fst) as Vector[]
    },
    score: manhattan,
    hash: ([x, y]: Vector) => `${x},${y}`,
  });

  for (const v of path) {
    if (!v.eq(start) && !v.eq(end)) grid.set(v, 2);
  }
  await timeout(100);
  await drawer(pg())
  return len;
}
await solveA(10, [1, 1], [7, 4]);

// %%

console.log("Sol A: ", await solveA(1358, [1, 1], [31,39]));

// %%

const solveB = async (fav: number, start: VecTuple, maxDist: number) => {
  const grid = new SparseGrid<number>();
  const tileType = (v: VecTuple) => isWall(v, fav) ? 0 : 1;
  grid.set(start, tileType(start));

  const gv2s = ['#', '.'];
  const pg = () => printGrid(grid, (_, v) => v === undefined ? ' ' : gv2s[v]);
  const drawer = await ulog(`solAout${fav}${start}${maxDist}`, pg())

  const nbs = (v: Vector) => {
    const nbrs = grid.neighbours(v);
    nbrs.forEach((nb) => {
      if (nb[1] === undefined) {
        nb[1] = tileType(nb[0]);
        grid.set(nb[0], nb[1]);
      }
    });
    drawer(pg())
    return nbrs.filter(([_, val]) => val! > 0).map(fst) as Vector[]
  };
  const hash = ([x, y]: VecTuple) => `${x},${y}`;

  const queue = new BinaryHeap<[Vector, number, string, Vector | null]>
  queue.push([new Vector(start), 0, hash(start), null])
  const seen = new Map<string, number>();
  seen.set(hash(start), 0);

  while (queue.length > 0) {
    const [tile, dist, hsh, parent] = queue.pop()!;

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
    await drawer(pg())
  }

  await timeout(100);
  await drawer(pg())
  return [...seen.values()].filter(v => v <= maxDist).length;
}

console.log("Sol B: ", await solveB(1358, [1, 1], 50));
