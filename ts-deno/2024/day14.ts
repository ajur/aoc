// %%
import { createCanvas } from "@gfx/canvas";
import { Vector, Vec, vx, vy, display, updateDisplay } from "#lib";
// %%

const sample = `
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3`
const sampleSize: Vec = [11, 7];

const data = await Deno.readTextFile("./data/day14.txt");
const dataSize: Vec = [101, 103];

type Robot = {p: Vector, v: Vector};

const parse = (s: string) => s.trim().split('\n').map(l => {
  const [_, x, y, vx, vy] = l.match(/p=(\d+),(\d+) v=([\d-]+),([\d-]+)/)!;
  return ({ p: new Vector(x.toInt(), y.toInt()), v: new Vector(vx.toInt(), vy.toInt()) } as Robot);
})
parse(sample)

// %%

const getQuadrant = (sv: Vec) => {
  const w = vx(sv), h = vy(sv);
  const qx1 = Math.floor(w / 2);
  const qx2 = w - qx1;
  const qy1 = Math.floor(h / 2);
  const qy2 = h - qy1;

  return (v: Vector) => {
    if (v.x < qx1 && v.y < qy1) return 'TL';
    if (v.x >= qx2 && v.y < qy1) return 'TR';
    if (v.x >= qx2 && v.y >= qy2) return 'BR';
    if (v.x < qx1 && v.y >= qy2) return 'BL';
    return 'X';
  }
}

const solveB = (s: string, size: Vec, moves: number) => parse(s)
  .map(({p, v}) => p.add(v.mult(moves)).modulo(size))
  .map(getQuadrant(size))
  .filter(s => s !== 'X')
  .sort()
  .callOnMe(ql => {
    const m = new Map<string, number>();
    for (const q of ql) {
      m.setdefault(q, 0);
      m.set(q, m.get(q)! + 1);
    }
    return m;
  })
  .values()
  .reduce((a, v) => a * v, 1)


solveB(sample, sampleSize, 100);
// %%

console.log("Sol A:", solveB(data, dataSize, 100));
// %%

const animateRobots = async (size: Vec, robots: Robot[] = []) => {
  const cellSize = 5;
  const spacing = 1;
  const cols = vx(size);
  const rows = vy(size);
  const canvasWidth = cols * (cellSize + spacing) + spacing;
  const canvasHeight = rows * (cellSize + spacing) + spacing;

  const canvas = createCanvas(canvasWidth, canvasHeight);
  const ctx = canvas.getContext("2d");

  const drawState = () => {
    ctx.fillStyle = "#333399";
    ctx.fillRect(0, 0, canvasWidth, canvasHeight);

    ctx.fillStyle = "#33aa33";
    for (const robot of robots) {
      const x = robot.p.x * (cellSize + spacing) + spacing;
      const y = robot.p.y * (cellSize + spacing) + spacing;
      ctx.fillRect(x, y, cellSize, cellSize);
    }
    return canvas;
  }

  await display(drawState());

  const center = Math.floor(cols / 2);

  for (let i = 0; i < 10000; ++i) {
    const onCenter: number[] = [];
    robots.forEach((r: Robot) => {
      r.p = r.p.add(r.v).modulo(size);
      if (r.p.x === center) onCenter.push(r.p.y);
    })
    onCenter.sort();

    let count = 0;
    let maxCount = 0;
    for (let k = 1; k < onCenter.length; ++k) {
      if (onCenter[k] === onCenter[k - 1] + 1) {
        ++count;
        maxCount = Math.max(maxCount, count);
      } else {
        count = 1
      }
    }

    if (maxCount > 10) {
      await updateDisplay(drawState());
      return i + 1;
    }
  }
}

console.log("Sol B:", await animateRobots(dataSize, parse(data)))
