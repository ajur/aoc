// %%
import { createCanvas } from "@gfx/canvas";
import { assertEquals } from "@std/assert/equals";
import { Grid, HashMap, isJupyter, Vector, repeat, lognb } from "./lib/index.ts";
// %%

const sample1 = `
AAAA
BBCD
BBCC
EEEC`
const sample2 = `
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO`
const sample3 = `
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE`

const data = await Deno.readTextFile('./data/day12.txt');

type FenceSide = 'N' | 'S' | 'E' | 'W';
type Plot = { crop: string, region: number, fences: FenceSide[] }

const parse = (s: string) => Grid.fromString(s.trim(), (c) => ({ crop: c, region: -1, fences: [] } as Plot));
parse(sample1)

// %%
type Region = [regId: number, area: number, fences: number];

const NBS_SIDES: FenceSide[] = ['W', 'N', 'E', 'S'];
const markRegion = (regId: number, v: Vector, g: Grid<Plot>): Region => {
  const visited = new HashMap<Vector, boolean>();
  const queue = [v];
  let fences = 0;
  let area = 0;
  while (queue.length > 0) {
    const next = queue.pop()!;
    if (visited.has(next)) continue;
    const plot = g.get(next)!;
    plot.region = regId;
    for (const [i, nbv] of next.orthogonals().entries()) {
      const nb = g.get(nbv);
      if (nb !== undefined && nb.crop === plot.crop) {
        queue.push(nbv);
      } else {
        plot.fences.push(NBS_SIDES[i]);
      }
    }
    fences += plot.fences.length;
    area += 1;
    visited.set(next, true);
  }
  return [regId, area, fences];
}

const markAllRegions = (g: Grid<Plot>): Region[] => {
  let nextRegionId = 0;
  const regions: Region[] = [];
  g.forEach((p, v) => {
    if (p && p.region >= 0) return;
    regions.push(markRegion(nextRegionId, v, g));
    ++nextRegionId;
  })
  return regions;
}
{
  const g = parse(sample1);
  lognb(markAllRegions(g));
  g;
}

// %%

const rainbow = (t: number, l = 0.5) => `hsl(${t * 360}, 100%, ${Math.floor(l * 100)}%)`

const printGarden = !isJupyter ? () => {} : (grid: Grid<Plot>, regions: number, cellSize = 15) => {
  const spacing = 0;
  const canvasWidth = grid.width * (cellSize + spacing) + spacing;
  const canvasHeight = grid.height * (cellSize + spacing) + spacing;

  const canvas = createCanvas(canvasWidth, canvasHeight);
  const ctx = canvas.getContext("2d");

  const regionsColors = repeat((i) => i * 1 / regions, regions).shuffle();

  ctx.fillStyle = "#000000";
  ctx.fillRect(0, 0, canvasWidth, canvasHeight);

  for (let row = 0; row < grid.rows; ++row) {
    for (let col = 0; col < grid.cols; ++col) {
      const plot = grid.get(col, row)!;
      ctx.fillStyle = rainbow(regionsColors[plot.region], 0.8);
      const x = col * (cellSize + spacing) + spacing;
      const y = row * (cellSize + spacing) + spacing;
      ctx.fillRect(x, y, cellSize, cellSize);
      ctx.fillStyle = rainbow(regionsColors[plot.region], 0.3);
      for (const fd of plot.fences) {
        switch(fd) {
          case 'W': ctx.fillRect(x + 1, y + 1, 1, cellSize - 2); break;
          case 'E': ctx.fillRect(x + cellSize - 2, y + 1, 1, cellSize - 2); break;
          case 'N': ctx.fillRect(x + 1, y + 1, cellSize - 2, 1); break;
          case 'S': ctx.fillRect(x + 1, y + cellSize - 2, cellSize - 2, 1); break;
        }
      }
    }
  }
  Deno.jupyter.display(canvas);
};
{
  const g = parse(sample1);
  const regions = markAllRegions(g);
  printGarden(g, regions.length, 25);
}

// %%

const solveA = (s: string, printCellSize = 25): number => {
  const g = parse(s);
  const regions = markAllRegions(g);
  printGarden(g, regions.length, printCellSize);
  return regions.map(([_, area, fences]) => area * fences).sum();
}

assertEquals(solveA(sample1), 140)
assertEquals(solveA(sample2), 772)
assertEquals(solveA(sample3), 1930)
// %%

console.log('Sol A:', solveA(data, 8));

// %%

const sample4 = `
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA`;

type RegionsWithSides = [...Region, sides: number];

const countSides = (g: Grid<Plot>, regions: Region[]): RegionsWithSides[] => {
  const regionSides = regions.map(() => 0);

  let prevPlot: Plot | undefined;

  const endedPrevFence = (current: Plot | undefined, side: FenceSide) =>
    (!current || current.region !== prevPlot?.region || current.fences.indexOf(side) < 0) && (prevPlot && prevPlot.fences.indexOf(side) >= 0);

  for (let y = 0; y < g.height; ++y) {
    for (let x = 0; x < g.width + 1; ++x) {
      const plot = g.get(x, y);
      // lognb('check', x, y, plot, 'ended N', endedPrevFence(plot, 'N'), 'ended S', endedPrevFence(plot, 'N'))
      if (endedPrevFence(plot, 'N')) regionSides[prevPlot!.region] += 1;
      if (endedPrevFence(plot, 'S')) regionSides[prevPlot!.region] += 1;
      prevPlot = plot;
    }
  }
  for (let x = 0; x < g.width; ++x) {
    for (let y = 0; y < g.height + 1; ++y) {
      const plot = g.get(x, y);
      // lognb('check', x, y, plot, 'ended W', endedPrevFence(plot, 'W'), 'ended E', endedPrevFence(plot, 'E'))
      if (endedPrevFence(plot, 'W')) regionSides[prevPlot!.region] += 1;
      if (endedPrevFence(plot, 'E')) regionSides[prevPlot!.region] += 1;
      prevPlot = plot;
    }
  }

  return regionSides.map((rs, i) => [...regions[i], rs] as RegionsWithSides);
}

const solveB = (s: string): number => {
  const g = parse(s);
  const regions = markAllRegions(g);
  const regionWithSides = countSides(g, regions);
  return regionWithSides.map(([_id, area, _fences, sides]) => area * sides).sum();
}

assertEquals(solveB(sample1), 80);
assertEquals(solveB(sample2), 436);
assertEquals(solveB(sample4), 368);
assertEquals(solveB(sample3), 1206);

// %%

console.log('Sol B:', solveB(data));
