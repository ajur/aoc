
import {functions as F, strings as S} from './utils.js'

export const sample = `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`

export const runA = (data) => parse(data)
  .map(nums => nums.peek())
  .apply(() => 0)

const parse = (str) => str.split('\n')
  .map(r => r.split(' ').map(S.asInt))