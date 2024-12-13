
import {grid as G, strings as S, arrays as A} from './utils.js'

export const sample = `
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
`

export const runA = (data) => parse(data)
  .map(({pos: [px, py], vel: [vx, vy]}) => ({
    // line parallel to vector V, going through point P: (x - Px)/Vx = (y - Py) / Vy
    // thus: Vy*x - Vx*y + Vx*Py - Vy*Px = 0  as parametric Ax+By+C=0
    a: vy,
    b: -vx,
    c: vx * py - vy * px,
    pos: [px, py],
    vel: [vx, vy]
  }))
  .apply(A.pairs)
  .map(([{a: a1, b: b1, c: c1, pos: [px1, py1], vel: vel1}, {a: a2, b: b2, c: c2, pos: [px2, py2], vel: vel2}]) => {
    // line intersection based on determinants
    const d = det(a1, b1, a2, b2);
    const dx = det(-c1, b1, -c2, b2);
    const dy = det(a1, -c1, a2, -c2);

    const intersecting = d !== 0;

    if (!intersecting) {
      return {intersecting}
    }

    const x = dx / d;
    const y = dy / d;

    const range = px1 < 100 ? [7, 27] : [200000000000000, 400000000000000]

    const inRange = pointInRange(range, [x, y]);

    const p1vec = [x - px1, y - py1];
    const p2vec = [x - px2, y - py2];

    const crossInFuture = vecSameDir(p1vec, vel1) && vecSameDir(p2vec, vel2);

    return {intersecting, x: dx / d, y: dy / d, crossInFuture, inRange}
  })
  .filter(({intersecting, crossInFuture, inRange}) => intersecting && crossInFuture && inRange)
  .length;

const det = (a, b, c, d) => a * d - b * c
const vecSameDir = (v1, v2) => Math.sign(v1[0]) === Math.sign(v2[0]) && Math.sign(v1[1]) === Math.sign(v2[1]);
const pointInRange = ([r0, r1], [px, py]) => r0 <= px && px <= r1 && r0 <= py && py <= r1;

const parse = (str) => str.split('\n')
  .map(S.split(' @ '))
  .map(([pos, vel]) => ({pos: pos.split(', ').map(S.asInt), vel: vel.split(', ').map(S.asInt)}))
