// %%

import { createCanvas } from "@gfx/canvas";
import { count, createGrid, getCol, getRow, map, mod, pipe, setCol, setRow, split, trim } from "./lib.ts";
import { assertEquals } from "@std/assert";

// %%

const WIDTH = 50;
const HEIGHT = 6;

const CELL_SIZE = 7;
const SPACING = 1;

const cWidth = WIDTH * (SPACING + CELL_SIZE) + SPACING;
const cHeight = HEIGHT * (SPACING + CELL_SIZE) + SPACING;
const canvas = createCanvas(cWidth, cHeight);
const ctx = canvas.getContext("2d");

const drawGrid = (grid: number[][]): ReturnType<typeof createCanvas> => {
  ctx.fillStyle = "#161456";
  ctx.fillRect(0, 0, cWidth, cHeight);

  for (let row = 0; row < grid.length; ++row) {
    for (let col = 0; col < grid[row].length; ++col) {
      ctx.fillStyle = (grid[row][col] > 0) ? "#A7DDFF" : "#2634E6";
      const x = col * (CELL_SIZE + SPACING) + 1;
      const y = row * (CELL_SIZE + SPACING) + 1;
      ctx.fillRect(x, y, CELL_SIZE, CELL_SIZE);
    }
  }

  return canvas;
}

drawGrid(createGrid(6, 6, (x, y) => x === y || x === 5 - y ? 1 : 0));

// %%

type CmdFn = (a: number, b: number) => (g: number[][]) => number[][];

const drawRect: CmdFn = (w, h) => (g: number[][]) => {
  for (let x = 0; x < w && x < g[0].length; ++x) {
    for (let y = 0; y < h && y < g.length; ++y) {
      g[y][x] = 1;
    }
  }
  return g;
}

const rotateRow: CmdFn = (r, n) => (g: number[][]) => {
  const arr = getRow(r, g);
  if (!arr) return g;
  const nn = mod(n, arr.length);
  const cutIdx = arr.length - nn;
  setRow(r, [...arr.slice(cutIdx), ...arr.slice(0, cutIdx)], g);
  return g;
}

const rotateCol: CmdFn = (c, n) => (g: number[][]) => {
  const arr = getCol(c, g);
  if (!arr) return g;
  const nn = mod(n, arr.length);
  const cutIdx = arr.length - nn;
  setCol(c, [...arr.slice(cutIdx), ...arr.slice(0, cutIdx)], g);
  return g;
}

const sampleGrid = createGrid(7, 3, 0);

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

const parse: (s: string) => ReturnType<CmdFn>[] = pipe(trim(), split('\n'), map(parseCmd), map(([cmdFn, a, b]: Cmd) => cmdFn(a, b)))

const findSolutionA = (g: number[][], s: string) => pipe(...parse(s))(g)

drawGrid(findSolutionA(createGrid(7, 3, 0), sample))

// %%

const solveA = (g: number[][], s: string) => count((n: number) => n > 0, findSolutionA(g, s))
assertEquals(6, solveA(createGrid(7, 3, 0), sample))


const data = await Deno.readTextFile('./data/day08.txt');

console.log("Sol A:", solveA(createGrid(WIDTH, HEIGHT, 0), data));

console.log("Sol B: ... in repl img")
drawGrid(findSolutionA(createGrid(WIDTH, HEIGHT, 0), data));
