import { createCanvas } from "@gfx/canvas";
import { lognb, Vector, VecTuple, isJupyter, display } from '#lib';
// %%

const sample =`
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
`;

type ParsedData = Vector[];
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => s.trim()
  .split('\n')
  .map(Vector.from);

lognb(parse(sample))

// %%

type DistSpec = [sqDist: number, v1idx: number, v2idx: number];

const solveA = (dat: ParsedData): [number, Vector, Vector] => {
  const dolog = dat.length < 10;
  const dists: DistSpec[] = [];
  for (let i = 0; i < dat.length; ++i) {
    for (let j = i + 1; j < dat.length; ++j) {
      const a = dat[i]
      const b = dat[j];
      dists.push([
        (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1),
        i, j]);
    }
  }
  dists.sort(([a], [b]) => b - a);
  dolog && lognb(dists);
  return [dists[0][0], dat[dists[0][1]], dat[dists[0][2]]];
}

lognb(solveA(parse(sample)));

// %%

const data = await Deno.readTextFile("./data/day09.txt");

console.log("Sol A:", solveA(parse(data))[0])

// %%

const boxCollideWithLine = (
  [b1x, b1y]: VecTuple, [b2x, b2y]: VecTuple,
  [l1x, l1y]: VecTuple, [l2x, l2y]: VecTuple
) => {
  if (l1x === l2x) { // vertical line
    if (Math.min(b1x, b2x) >= l1x || Math.max(b1x, b2x) <= l2x) return false;
    if (Math.max(l1y, l2y) <= Math.min(b1y, b2y) || Math.min(l1y, l2y) >= Math.max(b1y, b2y)) return false;
  } else { // horizontal line
    if (Math.min(b1y, b2y) >= l1y || Math.max(b1y, b2y) <= l2y) return false;
    if (Math.max(l1x, l2x) <= Math.min(b1x, b2x) || Math.min(l1x, l2x) >= Math.max(b1x, b2x)) return false;
  }
  return true;
}

// %%

const solveB = (dat: ParsedData): [number, Vector, Vector] => {
  const dists: DistSpec[] = [];
  for (let i = 0; i < dat.length; ++i) {
    for (let j = i + 1; j < dat.length; ++j) {
      const a = dat[i]
      const b = dat[j];
      dists.push([
        (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1),
        i, j]);
    }
  }
  dists.sort(([a], [b]) => b - a);

  for (let i = 0; i < dists.length; ++i) {
    const [area, ia, ib] = dists[i];
    const a = dat[ia];
    const b = dat[ib];

    let crossesAny = false;

    for (let j = 0; j < dat.length; ++j) {
      const va = dat.at(j - 1)!;
      const vb = dat[j];

      if (boxCollideWithLine(a, b, va, vb)) {
        crossesAny = true;
        break;
      }
    }
    if (!crossesAny) {
      return [area, a, b];
    }
  }

  return [0, Vector.ZERO, Vector.ZERO];
}

lognb(solveB(parse(sample)))
// %%

console.log("Sol B:", solveB(parse(data))[0])

// %%

if (isJupyter) {
  const printShape = async (vecs: ParsedData) => {
    let maxX = 0;
    let maxY = 0;
    for (const v of vecs) {
      if (v.x > maxX) maxX = v.x;
      if (v.y > maxY) maxY = v.y;
    }

    const scale = maxX > 10000 ? 0.01 : maxX > 1000 ? 0.1 : maxX < 100 ? 10 : 1;

    const s = (n: number) => Math.round(n * scale);

    const canvasWidth = Math.floor(s(maxX) + 10);
    const canvasHeight = Math.floor(s(maxY) + 10);

    const canvas = createCanvas(canvasWidth, canvasHeight);
    const ctx = canvas.getContext("2d");

    ctx.beginPath();
    ctx.moveTo(s(vecs.at(-1)!.x), s(vecs.at(-1)!.y));
    for (let i = 0; i < vecs.length; ++i) {
      const v = vecs[i];
      ctx.lineTo(s(v.x), s(v.y));
    }
    ctx.lineWidth = 1;
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#aaffaa';
    ctx.fill();
    ctx.stroke();

    const [_a, a1, a2] = solveA(vecs);
    ctx.strokeStyle = '#0000ff';
    ctx.strokeRect(s(Math.min(a1.x, a2.x)), s(Math.min(a1.y, a2.y)), s(Math.abs(a1.x - a2.x)), s(Math.abs(a1.y - a2.y)));

    const [_b, b1, b2] = solveB(vecs);
    ctx.strokeStyle = '#ff0000';
    ctx.strokeRect(s(Math.min(b1.x, b2.x)), s(Math.min(b1.y, b2.y)), s(Math.abs(b1.x - b2.x)), s(Math.abs(b1.y - b2.y)));

    return await display(canvas);
  }


  lognb("sample")
  await printShape(parse(sample));

  lognb("data")
  await printShape(parse(data));
}
