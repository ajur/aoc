
export type Vec = VecObject | VecTuple;
export type VecObject = {x: number, y: number};
export type VecTuple = [x: number, y: number];

const isVecTuple = (v: Vec): v is VecTuple => Array.isArray(v);

export const vx = (v: Vec) => isVecTuple(v) ? v[0] : v.x;
export const vy = (v: Vec) => isVecTuple(v) ? v[1] : v.y;
export const dist = (a: Vec, b: Vec): number => Math.hypot(vx(b) - vx(a), vy(b) - vy(a));
export const manhattan = (a: Vec, b: Vec): number => Math.abs(vx(b) - vx(a)) + Math.abs(vy(b) - vy(a));
export const neighbours = (v: Vec): Vector[] => [
  new Vector(vx(v) - 1, vy(v)),
  new Vector(vx(v), vy(v) - 1),
  new Vector(vx(v) + 1, vy(v)),
  new Vector(vx(v), vy(v) + 1)
];

const VecTupleConstructor: new (...args: VecTuple) => VecTuple = Array as any;

export class Vector extends VecTupleConstructor implements VecObject, VecTuple {
  constructor(x: number | Vec, y?: number) {
    super(
      typeof x === 'number' ? x : isVecTuple(x) ? x[0] : x.x,
      typeof x === 'number' ? y ?? x : isVecTuple(x) ? x[1] : x.y
    );
  }

  public get x() { return this[0]; }
  public set x(v: number) { this[0] = v; }
  public get y() { return this[1]; }
  public set y(v: number) { this[1] = v; }

  public eq(v: Vec) {
    return vx(v) === this.x && vy(v) === this.y;
  }
  public dist(v: Vec) {
    return Math.hypot(vx(v) - this[0], vy(v) - this[1]);
  }
  public manhattan(v: Vec) {
    return Math.abs(vx(v) - this[0]) + Math.abs(vy(v) - this[0]);
  }
  public neighbours() {
    return neighbours(this);
  }
}
