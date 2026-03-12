// %%
import { CanvasRenderingContext2D, createCanvas } from "@gfx/canvas";
import { asInt, lognb, display, gridFlipH, gridFlipV, gridRotateCW, Grid, isJupyter } from '#lib';
// %%

const sample =`
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
`;

type ShapeData = number[][];
const parseShape = (s: string): ShapeData => {
  const lines = s.trim().split('\n');
  const id = lines.shift()!.trim().split(':')[0].toInt() + 1; // offset id by 1, to use 0 as empty
  return lines.map(l => l.split('').map(c => c === '#' ? id : 0));
}

type SpaceData = {
  width: number;
  height: number;
  shapes: number[];
}
const parseSpace = (s: string): SpaceData => {
  const [l, r] = s.split(':');
  const [width, height] = l.trim().split('x').map(asInt);
  const shapes = r.trim().split(' ').map(asInt);
  return { width, height, shapes };
}

type ParsedData = {
  shapes: ShapeData[],
  spaces: SpaceData[]
};
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => {
  const blocks = s.trim().split('\n\n');
  const spaces = blocks.pop()!.trim().split('\n').map(parseSpace);
  const shapes = blocks.map(parseShape);
  return { shapes, spaces };
}
lognb(parse(sample));
// %%

const colors = ["#5c3f2f", "#9bc058", "#e9cc16", "#f6c0d0", "#e2464f", "#0a97b8", "#884b91", "#69bae1", "#d27ecd", "#bb62bf"];
const colorsDark = ["#3d2a1f", "#68803b", "#9d880f", "#a3808a", "#952f35", "#07657a", "#5a315f", "#477c96", "#8d5489", "#7d4180"];

class Shape {
  id: number;
  data: number[][];
  variants: number[][][];
  constructor(data: number[][]) {
    this.id = data[0].find(v => v > 0)! - 1;
    this.data = data;
    this.variants = this.#createVariants(data);
  }

  #createVariants(base: number[][]): number[][][] {
    const vs = [base];

    vs.push(gridRotateCW(base)); // 90
    vs.push(gridRotateCW(vs.at(-1)!)) // 180
    vs.push(gridRotateCW(vs.at(-1)!)) // 270
    vs.push(gridFlipH(base));
    vs.push(gridRotateCW(vs.at(-1)!))
    vs.push(gridRotateCW(vs.at(-1)!))
    vs.push(gridRotateCW(vs.at(-1)!))
    vs.push(gridFlipV(base));
    vs.push(gridRotateCW(vs.at(-1)!))
    vs.push(gridRotateCW(vs.at(-1)!))
    vs.push(gridRotateCW(vs.at(-1)!))

    const hash = (sh: number[][]) => sh.map(r => r.join('')).join('');
    const map = new Map(vs.map(v => [hash(v), v]));
    return [...map.values()];
  }

  drawVariant(ctx: CanvasRenderingContext2D, variantIdx: number, xOffset: number, yOffset: number, cellSize: number): void {
    const sh = this.variants[variantIdx];
    const clr = this.id + 1;
    ctx.fillStyle = colors[this.id + 1];
    ctx.strokeStyle = colorsDark[clr];
    ctx.lineWidth = 1;

    for (let yi = 0; yi < 3; ++yi) {
      for (let xi = 0; xi < 3; ++xi) {
        const clr = sh[yi][xi];
        if (clr > 0) {
          const x = xOffset + xi * cellSize;
          const y = yOffset + yi * cellSize;
          // fill box
          ctx.fillRect(x, y, cellSize, cellSize);
          // draw edges
          ctx.beginPath();
          if (!sh[yi][xi-1]) {
            ctx.moveTo(x, y);
            ctx.lineTo(x, y + cellSize);
          }
          if (!sh[yi][xi+1]) {
            ctx.moveTo(x + cellSize - 1, y);
            ctx.lineTo(x + cellSize - 1, y + cellSize);
          }
          if (!sh[yi-1]?.[xi]) {
            ctx.moveTo(x, y);
            ctx.lineTo(x + cellSize, y);
          }
          if (!sh[yi+1]?.[xi]) {
            ctx.moveTo(x, y + cellSize - 1);
            ctx.lineTo(x + cellSize, y + cellSize - 1);
          }
          ctx.stroke();
        }
      }
    }
  }

  async displayAllVariants({cellSize = 20} = {}) {
    const shapeSize = 3;
    const canvasWidth = ((shapeSize + 1) * this.variants.length + 1) * cellSize;
    const canvasHeight = (shapeSize + 2) * cellSize;

    const canvas = createCanvas(canvasWidth, canvasHeight);
    const ctx = canvas.getContext("2d");

    const clr = this.id + 1;
    ctx.fillStyle = colors[this.id + 1];
    ctx.strokeStyle = colorsDark[clr];
    ctx.lineWidth = 1;
    ctx.font = "14px sans-serif";

    for (let si = 0; si < this.variants.length; ++si) {
      const shapeX = (1 + (shapeSize + 1) * si) * cellSize;
      const shapeY = cellSize;
      ctx.fillText(`${this.id}:${si}`, shapeX, cellSize - 4)
      this.drawVariant(ctx, si, shapeX, shapeY, cellSize);
    }

    return await display(canvas);
  }
}

if (isJupyter) {
  const {shapes} = parse(sample);
  shapes.map(async (shd) => await new Shape(shd).displayAllVariants());
  undefined;
}

// %%

type BakedShapeData = [x: number, y: number, shapeId: number, shapeVariantIdx: number];

class State {
  shapesList: Shape[];
  grid: Grid<number>;
  shapesLeft: number[];

  shapes: BakedShapeData[];

  constructor(sd: SpaceData, shapes: Shape[]) {
    this.shapesList = shapes;
    this.grid = Grid.create(sd.width, sd.height, 0);
    this.shapesLeft = sd.shapes;
    this.shapes = [];
  }

  bakeShape(bsd: BakedShapeData): this {

    const [x, y, shid, shvar] = bsd;
    const shdata = this.shapesList[shid].variants[shvar];
    for (let sy = 0; sy < 3; ++sy) {
      for (let sx = 0; sx < 3; ++sx) {
        if (shdata[sy][sx] > 0) {
          this.grid.data[y + sy][x + sx] = shdata[sy][sx];
        }
      }
    }

    this.shapes.push(bsd);

    return this;
  }

  async displayState({cellSize = 20} = {}) {
    const canvasWidth = (this.grid.width + 2) * cellSize;
    const canvasHeight = (this.grid.height + 2) * cellSize;

    const canvas = createCanvas(canvasWidth, canvasHeight);
    const ctx = canvas.getContext("2d");

    ctx.fillStyle = colors[0];


    ctx.fillRect(cellSize, cellSize, this.grid.width * cellSize, this.grid.height * cellSize);

    for (let row = 0; row < this.grid.rows; ++row) {
      const x0 = cellSize;
      const x1 = canvasWidth - cellSize;
      const y = (row + 1) * cellSize;
      ctx.beginPath();
      ctx.moveTo(x0, y);
      ctx.lineTo(x1, y);
      ctx.moveTo(x0, y + cellSize - 1);
      ctx.lineTo(x1, y + cellSize - 1);
      ctx.stroke();
    }
    for (let col = 0; col < this.grid.cols; ++col) {
      const y0 = cellSize;
      const y1 = canvasHeight - cellSize;
      const x = (col + 1) * cellSize;
      ctx.beginPath();
      ctx.moveTo(x, y0);
      ctx.lineTo(x, y1);
      ctx.moveTo(x + cellSize - 1, y0);
      ctx.lineTo(x + cellSize - 1, y1);
      ctx.stroke();
    }

    for (let bsi = 0; bsi < this.shapes.length; ++bsi) {
      const [bsx, bsy, bsid, bsvar] = this.shapes[bsi];
      this.shapesList[bsid].drawVariant(
        ctx,
        bsvar,
        (1 + bsx) * cellSize,
        (1 + bsy) * cellSize,
        cellSize
      );
    }

    return await display(canvas);
  }
}

if (isJupyter) {
  const {spaces, shapes} = parse(sample);
  const shapesObjs = shapes.map(shd => new Shape(shd));
  await Promise.all(spaces.map((sp) =>new State(sp, shapesObjs).displayState()));

  lognb("now with some shapes");

  await new State(spaces[1], shapesObjs)
    .bakeShape([0, 0, 0, 1])
    .bakeShape([0, 2, 3, 0])
    .bakeShape([2, 2, 0, 2])
    .bakeShape([3, 0, 0, 1])
    .bakeShape([5, 2, 2, 0])
    .bakeShape([7, 0, 1, 0])
    .bakeShape([9, 1, 5, 0])
    .displayState();
  undefined;
}

// %%

// how to place shapes?
// - priority order left-right, top-down
// - to add next shape:
//   - if list is empty, pick most filled from top-left
//   - if there is space to the right of last space, try fit next shape
//   - otherwise go to next row of shapes, and try to fit to fist from previous line
// - to not keep all intermediate shapes, just last picked shape+variant combo,
//   list of shapes to check should be the same
//   - easiest would be to check it in order by id, but thats not optimal
//   - maybe better would be to precompute best fitting between all shapes and variants
//     - but that may not necessary be better :/

const solveA = (pd: ParsedData) => {}

lognb(solveA(parse(sample)));

// %%

const data = await Deno.readTextFile("./data/day12.txt");

if (isJupyter) {
  const {shapes} = parse(data);
  shapes.map(async (shd) => await new Shape(shd).displayAllVariants());
  undefined;
}

// %%

console.log("Sol A:", solveA(parse(data)))

// %%

const solveB = (pd: ParsedData) => {}

lognb(solveB(parse(sample)))
// %%

console.log("Sol B:", solveB(parse(data)))
