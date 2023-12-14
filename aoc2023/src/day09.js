
import {functions as F, strings as S} from './utils.js'

export const sample = `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`

export const runA = (data) => parse(data)
  .map(a => {
    const aa = [a];
    while (aa.at(-1).some(n => n !== 0)) {
      aa.push(diffs(aa.at(-1)));
    }
    return aa.reduceRight((acc, cur) => cur.at(-1) + acc, 0);
  })
  .sum()

export const runB = (data) => parse(data)
  .map(a => {
    const aa = [a];
    while (aa.at(-1).some(n => n !== 0)) {
      aa.push(diffs(aa.at(-1)));
    }
    return aa.reduceRight((acc, cur) => cur.at(0) - acc, 0);
  })
  .sum()

const diffs = a => {
  const out = [];
  for (let i = 1; i < a.length; ++i) {
    out.push(a[i] - a[i - 1]);
  }
  return out;
}

const parse = (str) => str.split('\n')
  .map(r => r.split(' ').map(S.asInt))