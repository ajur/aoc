// %%

import { createCanvas } from "@gfx/canvas";
import { assertEquals } from "@std/assert";
import { Grid, gt, pipe } from "#lib";

// %%

const WIDTH = 50;
const HEIGHT = 6;

const CELL_SIZE = 7;
const SPACING = 1;

const cWidth = WIDTH * (SPACING + CELL_SIZE) + SPACING;
const cHeight = HEIGHT * (SPACING + CELL_SIZE) + SPACING;
const canvas = createCanvas(cWidth, cHeight);
const ctx = canvas.getContext("2d");

const drawGrid = (grid: Grid<number>): ReturnType<typeof createCanvas> => {
  ctx.fillStyle = "#161456";
  ctx.fillRect(0, 0, cWidth, cHeight);

  for (let row = 0; row < grid.rows; ++row) {
    for (let col = 0; col < grid.cols; ++col) {
      const val = grid.get(col, row) ?? 0;
      ctx.fillStyle = (val > 0) ? "#A7DDFF" : "#2634E6";
      const x = col * (CELL_SIZE + SPACING) + 1;
      const y = row * (CELL_SIZE + SPACING) + 1;
      ctx.fillRect(x, y, CELL_SIZE, CELL_SIZE);
    }
  }

  return canvas;
}

drawGrid(Grid.create(6, 6, ([x, y]) => x === y || x === 5 - y ? 1 : 0));

// %%

type CmdFn = (a: number, b: number) => (g: Grid<number>) => Grid<number>;

const drawRect: CmdFn = (w, h) => (g: Grid<number>) => {
  for (let x = 0; x < w && x < g.width; ++x) {
    for (let y = 0; y < h && y < g.height; ++y) {
      g.set([x, y], 1);
    }
  }
  return g;
}

const rotateRow: CmdFn = (r, n) => (g: Grid<number>) => {
  const arr = g.getRow(r);
  if (!arr) return g;
  const nn = Math.mod(n, arr.length);
  const cutIdx = arr.length - nn;
  g.setRow(r, [...arr.slice(cutIdx), ...arr.slice(0, cutIdx)]);
  return g;
}

const rotateCol: CmdFn = (c, n) => (g: Grid<number>) => {
  const arr = g.getCol(c);
  if (!arr) return g;
  const nn = Math.mod(n, arr.length);
  const cutIdx = arr.length - nn;
  g.setCol(c, [...arr.slice(cutIdx), ...arr.slice(0, cutIdx)]);
  return g;
}

const sampleGrid = Grid.create(7, 3, 0);

pipe(drawRect(4, 2), rotateRow(1, 1), rotateCol(3, 2), drawGrid)(sampleGrid)


// %%

const sample = `
rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1`

type Cmd = [cmd: CmdFn, a: number, b: number];

const parseCmd = (s: string): Cmd => {
  const parts = s.split(' ');
  if (parts[0] === 'rect') {
    const [a, b] = parts[1].split('x');
    return [drawRect, parseInt(a), parseInt(b)];
  } else if (parts[0] === 'rotate') {
    if (parts[1] === 'row') {
      return [rotateRow, parseInt(parts[2].split('=')[1]), parseInt(parts[4])]
    } else if (parts[1] === 'column') {
      return [rotateCol, parseInt(parts[2].split('=')[1]), parseInt(parts[4])]
    }
  }
  throw new Error(`couldnt parse: ${s}`)
}

const parse = (s: string): ReturnType<CmdFn>[] => s.trim().split('\n').map(parseCmd).map(([cmdFn, a, b]: Cmd) => cmdFn(a, b));

const findSolutionA = (g: Grid<number>, s: string): Grid<number> => pipe(...parse(s))(g)

drawGrid(findSolutionA(Grid.create(7, 3, 0), sample))

// %%

const solveA = (g: Grid<number>, s: string) => findSolutionA(g, s).count(gt(0))

assertEquals(6, solveA(Grid.create(7, 3, 0), sample))

// %%

const data = await Deno.readTextFile('./data/day08.txt');

console.log("Sol A:", solveA(Grid.create(WIDTH, HEIGHT, 0), data));

const sol = findSolutionA(Grid.create(WIDTH, HEIGHT, 0), data);
console.log("Sol B:\n" + sol.pprint(n => n ? '█' : ' '))
drawGrid(sol);
