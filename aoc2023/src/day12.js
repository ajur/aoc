
import {functions as F, strings as S, arrays as A} from './utils.js'

export const sample = `
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
`

export const runA = (data) => parse(data)
  .map(findArrangments)
  .sum()
  
export const runB = (data) => parse(data)
  .map(({springs, spec, outOf}, idx, t) => ({
    springs: A.repeat(5, springs).join('?'),
    spec: A.repeat(5, spec).flat(),
    outOf: '' + (idx + 1) + '/' + t.length
  }))
  .map(findArrangments)
  .sum()


// better but not working untill given memo... welp forgot about it at the start, ofcourse...
const findArrangments = (({springs, spec, outOf}) => {

  // console.log(outOf, ">", springs, spec)
  
  const isLast = (specIdx) => specIdx === spec.length - 1;

  const cbpCache = [];

  const canBePlaced = memo3((specIdx, atPos, offset) => {
    const d = spec[specIdx];
    for (let o = -1; o < offset; ++o) {
      if (springs[atPos + o] === '#') return false;
    }
    const pos = atPos + offset;
    for (let i = 0; i < d; ++i) {
      if ((pos + i) >= springs.length || springs[pos + i] === '.') return false;
    }
    const ending = !isLast(specIdx) ? (pos + d + 1) : springs.length;
    for (let e = pos + d; e < ending; ++e) {
      if (springs[e] === '#') return false;
    }
    return true;
  })

  const findForSpec = memo3((specIdx, atPos, maxOffset) => {
    let out = 0;
    if (isLast(specIdx)) {
      for (let offset = 0; offset <= maxOffset; ++offset) {
        if (canBePlaced(specIdx, atPos, offset)) {
          // console.log('<<< specIdx', specIdx, 'atPos', atPos, 'offset', offset);
          out += 1;
        }
      }
      return out;
    }
    
    const d = spec[specIdx];
    for (let offset = 0; offset <= maxOffset; ++offset) {
      if (canBePlaced(specIdx, atPos, offset)) {
        const subspec = findForSpec(specIdx + 1, atPos + offset + d + 1, maxOffset - offset);
        // if (subspec > 0) console.log('>>> specIdx', specIdx, 'atPos', atPos, 'offset', offset, 'subs', subspec);
        out += subspec
      }
    }
    return out;
  })

  const maxOffset = springs.length - spec.sum() - spec.length + 1;
  // console.log("> maxOffset", maxOffset);

  const arrangments = findForSpec(0, 0, maxOffset);
  // console.log("> arrangments", arrangments)

  // console.log(outOf, '=>', arrangments);
  return arrangments;
});

const memo3 = (f) => {
  const cache = [];
  return (a,b,c) => {
    if (cache[a]?.[b]?.[c] !== undefined) return cache[a][b][c];
    const out = f(a,b,c);
    cache[a] ??= [];
    cache[a][b] ??= [];
    cache[a][b][c] = out;
    return out;
  }
}

const parse = (str) => str.split('\n')
  .map(S.split(' '))
  .map(([s, ns]) => ({
    springs: s,
    spec: ns.split(',').map(S.asInt)
  }))
  