
Object.prototype.apply = function(f) {
  return f(this);
}
Object.prototype.peek = function (f) {
  f(this);
  return this;
}
Object.prototype.log = function (msg) {
  if (msg) console.log(msg, this)
  else console.log(this);
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

String.prototype.toInt = function() {
  return parseInt(this, 10);
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
  desc: (m = functions.id) => (a, b) => m(b) - m(a)
}

export const strings = {
  asInt: (s) => parseInt(s, 10)
}

export const maths = {
  gcd: (a, b) => (b == 0) ? a : maths.gcd(b, a % b),
  lcm: (...n) => n.reduce((a,b) => a / maths.gcd(a, b) * b),
  inRange: (a, b) => v => Math.min(a, b) <= v && v < Math.max(a, b)
}

export const arrays = {
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
  at: (g, r, c) => g?.[r]?.[c],
  set: (g, r, c, v) => {
    g[r][c] = v
  },
  find: (g, p) => {
    for(let r = 0; r < g.length; ++r) {
      for(let c = 0; c < g[r].length; ++r) {
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
  }
}