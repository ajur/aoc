import { notNull } from "./misc.ts";
import { Vector, Vec, vy, vx, neighbours } from "./vector.ts";
import "./math.ts";

export const transpose = <T>(g: T[][]): T[][] => {
  const rows = g.length;
  const cols = g[0].length;
  const out = new Array(cols);
  for (let col = 0; col < cols; ++col) {
    out[col] = new Array(rows);
    for (let row = 0; row < rows; ++row) {
      out[col][row] = g[row][col];
    }
  }
  return out;
};

export const isWellFormdGrid = (g: unknown[][]): boolean => {
  if (!Array.isArray(g) || g.length === 0 || !Array.isArray(g[0]) || g[0].length === 0) return false;
  const l0 = g[0].length;
  for (let r = 1; r < g.length; ++r) {
    if (g[r].length !== l0) return false;
  }
  return true;
}

export type GridMapper<T, V> = (value: T | undefined, vec: Vector, arr: Grid<T>) => V;
export type GridPredicate<T> = GridMapper<T, boolean>;

export type CreateFromStringOpts<T> = {
  mapper?: (value: string, vec: Vector) => T,
  sepCol?: string,
  sepRow?: string,
}

export class Grid<T> {
  data: T[][];
  rows: number;
  cols: number;

  constructor(d?: T[][]) {
    this.data = d ?? [];
    this.rows = this.data.length;
    this.cols = this.rows === 0 ? 0 : Math.max(...this.data.map(r => r.length));
  }

  static create<V>(cols: number, rows: number, valOrMapper: ((v: Vector) => V) | V): Grid<V> {
    const mapper = typeof valOrMapper === "function" ? (valOrMapper as (v: Vector) => V) : () => valOrMapper;
    return new Grid(Array.from({length: rows}, (_, r) => Array.from({length: cols}, (_, c) => mapper(new Vector(c, r)))));
  }

  static fromString<V>(str: string, mapper: (value: string, vec: Vector) => V, sepCol = '', sepRow = '\n',): Grid<V> {
    return new Grid(str
      .split(sepRow ?? '\n')
      .map((row, ridx) => row
        .split(sepCol ?? '')
        .map((c, cidx) => mapper(c, new Vector(cidx, ridx))))
    );
  }

  get width() { return this.cols; }
  get height() { return this.rows; }

  get(v: Vec): T | undefined;
  get(x: number, y: number): T | undefined;
  get(x: Vec | number, y?: number): T | undefined {
    const xx = typeof x === "number" ? x : vx(x);
    const yy = typeof y === "number" ? y : typeof x === "number" ? x : vy(x);
    return this.data[yy]?.[xx];
  }

  set(v: Vec, val: T): Grid<T> {
    const x = vx(v);
    const y = vy(v);

    this.data[y] ??= [];
    this.data[y][x] = val;

    if (x >= this.cols) this.cols = x + 1;
    if (y >= this.rows) this.rows = y + 1;

    return this;
  }

  at(v: Vec): T | undefined;
  at(x: number, y: number): T | undefined;
  at(x: Vec | number, y?: number): T | undefined {
    const xx = typeof x === "number" ? x : vx(x);
    const yy = typeof y === "number" ? y : typeof x === "number" ? x : vy(x);
    return this.data[Math.mod(yy, this.rows)]?.[Math.mod(xx, this.cols)];
  }

  clone() {
    return new Grid(this.data.map(row => [...row]));
  }

  *[Symbol.iterator]() {
    for (let row = 0; row < this.rows; ++row) {
      for (let col = 0; col < this.cols; ++col) {
        yield this.data[row]?.[col];
      }
    }
  }

  transpose(): Grid<T> {
    const out: T[][] = [];
    for (let col = 0; col < this.cols; ++col) {
      const outRow: T[] = [];
      for (let row = 0; row < this.rows; ++row) {
        if (this.data[row]?.[col] !== undefined) {
          outRow[row] = this.data[row][col];
        }
      }
      out.push(outRow);
    }
    return new Grid(out);
  }

  neighbours(v: Vec): [Vector, T | undefined][] {
    return neighbours(v)
      .filter((nv) => nv.x >= 0 && nv.y >= 0)
      .map((nv) => [nv, this.get(nv)]);
  }

  map<V>(m: GridMapper<T, V>): Grid<V> {
    const out = new Grid<V>();
    for (let row = 0; row < this.rows; ++row) {
      for (let col = 0; col < this.cols; ++col) {
        const v = new Vector(col, row);
        const val = m(this.data[row]?.[col], v, this);
        if (v !== undefined) {
          out.set(v, val);
        }
      }
    }
    return out;
  }

  count(p: GridPredicate<T> = notNull): number {
    let c = 0;
    for (let row = 0; row < this.rows; ++row) {
      for (let col = 0; col < this.cols; ++col) {
        if (p(this.data[row]?.[col], new Vector(col, row), this)) ++c;
      }
    }
    return c;
  }

  getRow(idx: number): T[] | undefined {
    if (idx >= 0 && idx < this.rows) {
      return [...this.data[idx]];
    }
  }

  setRow(idx: number, v: T[]): Grid<T> {
    if (idx < 0) idx = Math.mod(idx, this.rows);
    if (idx >= this.rows) {
      this.rows = idx + 1;
    }
    this.data[idx] ??= [];
    for (let i = 0; i < this.cols, i < v.length; ++i) {
      this.data[idx][i] = v[i];
    }
    return this;
  }

  getCol(idx: number): T[] | undefined {
    if (idx >= 0 && idx < this.cols) {
      return this.data.map(a => a[idx]);
    }
  }

  setCol(idx: number, v: T[]): Grid<T> {
    if (idx < 0) idx = Math.mod(idx, this.cols);
    for (let i = 0; i < this.rows, i < v.length; ++i) {
      this.data[i] ??= [];
      this.data[i][idx] = v[i];
    }
    return this;
  }

  indexOf(v: T): Vector | undefined {
    for (let y = 0; y < this.rows; ++y) {
      for (let x = 0; x < this.cols; ++x) {
        if (this.data[y]?.[x] === v) {
          return new Vector(x, y);
        }
      }
    }
  }

  findIndex(p: GridPredicate<T>): Vector | undefined {
    for (let y = 0; y < this.rows; ++y) {
      for (let x = 0; x < this.cols; ++x) {
        const v = new Vector(x, y);
        if (p(this.data[y]?.[x], v, this)) {
          return v;
        }
      }
    }
  }
  *findAllIndexes(p: GridPredicate<T>) {
    for (let y = 0; y < this.rows; ++y) {
      for (let x = 0; x < this.cols; ++x) {
        const v = new Vector(x, y);
        if (p(this.data[y]?.[x], v, this)) {
          yield v;
        }
      }
    }
  }
  find(p: GridPredicate<T>): T | undefined {
    for (let y = 0; y < this.rows; ++y) {
      for (let x = 0; x < this.cols; ++x) {
        const v = new Vector(x, y);
        const val = this.data[y]?.[x];
        if (p(val, v, this)) {
          return val;
        }
      }
    }
  }
  *findAll(p: GridPredicate<T>) {
    for (let y = 0; y < this.rows; ++y) {
      for (let x = 0; x < this.cols; ++x) {
        const v = new Vector(x, y);
        const val = this.data[y]?.[x];
        if (p(this.data[y]?.[x], v, this)) {
          yield val;
        }
      }
    }
  }

  pprint(m?: GridMapper<T, string>): string {
    const mapper: GridMapper<T, string> = m ?? ((v: T | undefined) => `${v}`);
    const sa: string[] = [];
    for (let r = 0; r < this.rows; ++r) {
      for (let c = 0; c < this.cols; ++c) {
        const v = new Vector(c, r);
        sa.push(mapper(this.get(v), v, this))
      }
      sa.push('\n');
    }
    return sa.join('');
  }
}
