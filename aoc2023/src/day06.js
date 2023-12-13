
import {strings as S} from './utils.js'

export const sample = `
Time:      7  15   30
Distance:  9  40  200
`

export const runA = (data) => parse(data)
  // .peek()
  .map(calcRange)
  // .peek()
  .map(intsInRange)
  // .peek()
  .reduce((agg, n) => agg * n);

export const runB = (data) => parseB(data)
  // .peek()
  .map(calcRange)
  // .peek()
  .map(intsInRange)
  // .peek()
  .reduce((agg, n) => agg * n)

const intsInRange = ([a, b]) => Math.ceil(b - 1) - Math.floor(a + 1) + 1

const calcRange = ({time, dist}) => {
  // Who remembers how to solve equations? WolframAlpha xD
  // https://www.wolframalpha.com/input?i=x*(t-x)+-+d+%3E+0
  const s = Math.sqrt(time * time - 4.0 * dist);
  return [
    (time - s) / 2.0,
    (time + s) / 2.0
  ];
};

const parse = (str) => {
  const rows = str.split('\n').map(row => row.split(/\s+/));
  const games = [];
  for (let i = 1; i < rows[0].length; ++i) {
    games.push({time: S.asInt(rows[0][i]), dist: S.asInt(rows[1][i])});
  }
  return games;
}

const parseB = (str) => {
  const [time, dist] = str
    .replace('Time:', '')
    .replace('Distance', '')
    .replaceAll(/\s+/g, '')
    .split(':')
    .map(S.asInt);
  return [{time, dist}];
}
