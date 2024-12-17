import { $hash, Hashable } from "./common.ts";
import './math.ts';

export type Vec = VecObject | VecTuple;
export type VecObject = {x: number, y: number};
export type VecTuple = [x: number, y: number];

const isVecTuple = (v: Vec): v is VecTuple => Array.isArray(v) && v.length === 2;

export const vx = (v: Vec) => isVecTuple(v) ? v[0] : v.x;
export const vy = (v: Vec) => isVecTuple(v) ? v[1] : v.y;
export const dist = (a: Vec, b: Vec): number => Math.hypot(vx(b) - vx(a), vy(b) - vy(a));
export const manhattan = (a: Vec, b: Vec): number => Math.abs(vx(b) - vx(a)) + Math.abs(vy(b) - vy(a));

export const orthogonals = (v: Vec): Vector[] => {
  const x = vx(v), y = vy(v);
  return [
    new Vector(x - 1, y),
    new Vector(x, y - 1),
    new Vector(x + 1, y),
    new Vector(x, y + 1)
  ];
};
export const diagonals = (v: Vec): Vector[] => {
  const x = vx(v), y = vy(v);
  return [
    new Vector(x - 1, y - 1),
    new Vector(x + 1, y - 1),
    new Vector(x + 1, y + 1),
    new Vector(x - 1, y + 1)
  ];
}
export const neighbours = (v: Vec): Vector[] => {
  const o = orthogonals(v);
  const d = diagonals(v);
  return [o[0], d[0], o[1], d[1], o[2], d[2], o[3], d[3]];
}

// deno-lint-ignore no-explicit-any
const VecTupleConstructor: new (...args: VecTuple) => VecTuple = Array as any;

export class Vector extends VecTupleConstructor implements VecObject, VecTuple, Hashable {
  constructor(x: number | Vec, y?: number) {
    super(
      typeof x === 'number' ? x : vx(x),
      typeof x === 'number' ? y ?? x : vy(x)
    );
  }

  clone() {
    return new Vector(this[0], this[1]);
  }

  get x() { return this[0]; }
  set x(v: number) { this[0] = v; }
  get y() { return this[1]; }
  set y(v: number) { this[1] = v; }

  override toString() {
    return `Vector(${this[0]},${this[1]})`;
  }
  [$hash]() {
    return this[0] + ',' + this[1];
  }

  eq(v: Vec) {
    return vx(v) === this.x && vy(v) === this.y;
  }

  dist(v: Vec) {
    return Math.hypot(vx(v) - this[0], vy(v) - this[1]);
  }
  manhattan(v: Vec) {
    return Math.abs(vx(v) - this[0]) + Math.abs(vy(v) - this[0]);
  }

  orthogonals() { return orthogonals(this); }
  diagonals() { return diagonals(this); }
  neighbours() { return neighbours(this); }

  add(v: Vec | number) {
    if (typeof v === "number") return new Vector(this[0] + v, this[1] + v);
    if (isVecTuple(v)) return new Vector(this[0] + v[0], this[1] + v[1]);
    return new Vector(this[0] + v.x, this[1] + v.y);
  }
  sub(v: Vec | number) {
    if (typeof v === "number") return new Vector(this[0] - v, this[1] - v);
    if (isVecTuple(v)) return new Vector(this[0] - v[0], this[1] - v[1]);
    return new Vector(this[0] - v.x, this[1] - v.y);
  }
  mult(v: number | Vec) {
    if (typeof v === "number") return new Vector(this[0] * v, this[1] * v);
    return new Vector(this[0] * vx(v), this[1] * vy(v));
  }
  div(v: number | Vec) {
    if (typeof v === "number") return new Vector(this[0] / v, this[1] / v);
    return new Vector(this[0] / vx(v), this[1] / vy(v));
  }
  neg() {
    return this.mult(-1);
  }
  modulo(v: number | Vec) {
    const mx = typeof v === "number" ? v : vx(v);
    const my = typeof v === "number" ? v : vy(v);
    return new Vector(Math.mod(this[0], mx), Math.mod(this[1], my));
  }

  rotate(a: number) {
    const cosA = Math.cos(a);
    const sinA = Math.sin(a);
    return new Vector(
      cosA * this[0] - sinA * this[1],
      sinA * this[0] + cosA * this[1]
    );
  }

  round() {
    return new Vector(Math.round(this[0]), Math.round(this[1]));
  }
}


export class Direction extends Vector {
  name: string = '?';
  char: string = '?';
  private constructor(x: number | Vec, y?: number, name?: string, char?: string) {
    super(
      Math.sign(typeof x === 'number' ? x : vx(x)),
      Math.sign(typeof x === 'number' ? y ?? x : vy(x))
    );
    if (name) {
      this.name = name;
      Direction.str2dir.set(name, this);
    }
    if (char) {
      this.char = char;
      Direction.str2dir.set(char, this);
    }
    Direction.str2dir.set(this[$hash](), this);
  }

  static str2dir = new Map<string, Direction>();

  static N = new Direction(0, -1, 'N', '^');
  static S = new Direction(0, 1, 'S', 'v');
  static W = new Direction(-1, 0, 'W', '<');
  static E = new Direction(1, 0, 'E', '>');
  static NW = new Direction(-1, -1, 'NW', 'F');
  static NE = new Direction(1, -1, 'NE', '7');
  static SE = new Direction(1, 1, 'SE', 'J');
  static SW = new Direction(-1, 1, 'SW', 'L');

  static from(c: string | Vector | Vec): Direction | undefined {
    const hsh = typeof c === 'string' ? c : c instanceof Vector ? c[$hash]() : (vx(c) + ',' + vy(c));
    return Direction.str2dir.get(hsh);
  }

  fw() { return this; }
  left() { return Direction.from(this.rotate(-Math.PI / 2).round())!; }
  right() { return Direction.from(this.rotate(Math.PI / 2).round())!; }
  back() { return Direction.from(this.neg())!; }
  fwLeft() { return Direction.from(this.rotate(-Math.PI / 4).round())!; }
  fwRight() { return Direction.from(this.rotate(Math.PI / 4).round())!; }
  backLeft() { return Direction.from(this.rotate(- 3 * (Math.PI / 4)).round())!; }
  backRight() { return Direction.from(this.rotate(3 * (Math.PI / 4)).round())!; }
}
