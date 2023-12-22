
import {Array2D} from 'array2d';

Object.prototype.apply = function(f) {
  return f(this);
}
Object.prototype.peek = function (f) {
  f(this);
  return this;
}
Object.prototype.log = function (msg) {
  if (typeof msg === 'string') console.log(msg, this);
  else console.log(this);
  return this;
}
Object.prototype.logj = function (msg, n = 2) {
  if (typeof msg === 'string') console.log(msg, JSON.stringify(this, null, n));
  else console.log(JSON.stringify(this, null, n));
  return this;
}

Object.filter = (obj, predicate) => Object.fromEntries(Object.entries(obj).filter(predicate));

Array.prototype.sum = function() {
  let sum = 0;
  for (let i = 0; i < this.length; i++) {
      sum += this[i];
  }
  return sum;
}
Array.prototype.inGroupOf = function(n) {
  const arr = [];
  for (let i = 0; i < this.length; i += n) {
      arr.push(this.slice(i, i + n));
  }
  return arr;
}
Array.prototype.groupSame = function(n) {
  const arr = [];
  let last = '';
  for (let i = 0; i < this.length; ++i) {
    if (this[i] === last) {
      arr.at(-1)[1] += 1;
    } else {
      last = this[i];
      arr.push([last, 1]);
    }
  }
  return arr;
}
Array.prototype.intersection = function (arr) {
  return this.filter(x => arr.indexOf(x) !== -1);
}
Array.prototype.unique = function () {
  return this.filter((x, i) => this.indexOf(x) === i);
}
Array.prototype.sortAsc = function () {
  return this.sort((a, b) => a - b);
}
Array.prototype.sortDesc = function () {
  return this.sort((a, b) => b - a);
}
Array.prototype.zip = function (...ars) {
  const arrs = [this, ...ars];
  const out = new Array(this.length);
  for (let i = 0; i < out.length; ++i) {
    out[i] = arrs.map(functions.nth(i));
  }
  return out;
}
Array.prototype.count = function (p) {
  return this.reduce((s, v) => p(v) ? s + 1 : s, 0);
}

const builtin_trim = String.prototype.trim;
String.prototype.trim = function(c) {
  if (!c) return builtin_trim.call(this);
  let a = 0;
  let b = this.length;
  const n = c.length;
  while (this.startsWith(c, a)) a += n;
  while (this.endsWith(c, b)) b -= n;
  return this.substring(a, b);
}

export const functions = {
  compose: (...fns) => x => fns.reduceRight((v, f) => f(v), x),
  pipe: (...fns) => x => fns.reduce((v, f) => f(v), x),
  id: o => o,
  fst: arr => arr[0],
  snd: arr => arr[1],
  last: arr => arr.at(-1),
  nth: n => arr => arr[n],
  key: k => o => o[k],
  asc: (m = functions.id) => (a, b) => m(a) - m(b),
  desc: (m = functions.id) => (a, b) => m(b) - m(a),
  is: a => b => a === b
}

export const strings = {
  asInt: (s) => parseInt(s, 10),
  split: by => s => s.split(by),
  join: by => as => as.join(by)
}

export const maths = {
  gcd: (a, b) => (b == 0) ? a : maths.gcd(b, a % b),
  lcm: (...n) => n.reduce((a,b) => a / maths.gcd(a, b) * b),
  inRange: (a, b) => v => Math.min(a, b) <= v && v < Math.max(a, b),
  shoelace: (points) => Math.abs(0.5 * points.reduce((sum, [x1, y1], i) => {
    const [x2, y2] = points[(i + 1) % points.length];
    return sum + x1 * y2 - x2 * y1
  }, 0)),
  manhatan: ([x1, y1], [x2, y2]) => Math.abs(x2 - x1) + Math.abs(y2 - y1),
  dist: ([x1, y1], [x2, y2]) => Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2),
  circumference: (points) => points.reduce((sum, p1, i) => sum + maths.dist(p1, points[(i + 1) % points.length]), 0),
  area: (points, withPath = true) => maths.shoelace(points) + 1 + (withPath ? 0.5 : -0.5) * maths.circumference(points),
}

export const arrays = {
  repeat: (n, v) => Array.from({length: n}, () => v),
  range: (a, b = 0) => {
    const aa = Math.min(a,b);
    const bb=Math.max(a,b)
    const n = bb-aa;
    return Array.from({length: n}, (_, i) => aa + i)
  },
  from: (n, f) => Array.from({length: n}, (_, i) => f(i)),
  pairs: a => {
    const ps = [];
    for (let i = 0; i < a.length; ++i) {
      for (let j = i + 1; j < a.length; ++j) {
        ps.push([a[i], a[j]]);
      }
    }
    return ps;
  }
}

export const grid = {
  create: Array2D.build,
  map: Array2D.map,
  forEach: Array2D.eachCell,
  forArea: Array2D.forArea,
  copy: (g) => g.map(r => [...r]),
  at: (g, r, c) => g?.[r]?.[c],
  set: (g, r, c, v) => {
    g[r][c] = v
  },
  eq: (key = functions.id) => (g1, g2) => {
    if (g1.length !== g2.length) return false;
    for (let row = 0; row < g1.length; ++row) {
      if (g1[row].length !== g2[row].length) return false;
      for (let col = 0; col < g1[row].length; ++col) {
        if (key(g1[row][col]) !== key(g2[row][col])) return false;
      }
    }
    return true;
  },
  find: (g, p) => {
    for(let r = 0; r < g.length; ++r) {
      for(let c = 0; c < g[r].length; ++c) {
        if (p(g[r][c], r, c, g)) {
          return [r, c];
        }
      }
    }
  },
  findAll: (g, p) => {
    const found = [];
    for(let r = 0; r < g.length; ++r) {
      for(let c = 0; c < g[r].length; ++c) {
        if (p(g[r][c], r, c, g)) {
          found.push([r, c]);
        }
      }
    }
    return found;
  },
  transpose: (g) => {
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
  },
  neighbours: (row, col) => [[row - 1, col], [row, col - 1], [row + 1, col], [row, col + 1]],
  pprint: (p = v => '' + v) => (g) => g.map(r => r.map(p).join('')).join('\n'),
  count: (g, p) => g.map(r => r.count(p)).sum(),
  flatMap: (g, f) => g.flatMap((r, ri) => r.map((v, ci) => f(v, ri, ci)).reduce((a, b) => a.map((ar,i) => [...ar, ...b[i]])))
}
