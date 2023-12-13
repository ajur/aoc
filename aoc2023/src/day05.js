
import {strings as S, functions as F} from './utils.js'

export const sample = `
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
`

export const runA = (data) => {
  const {seeds, maps} = parse(data)
  // console.log(maps[0].header, '20->20', mapper0(20) == 20, '60->62', mapper0(60) == 62, 'edges', mapper0(98) == 50 && mapper0(99) == 51 && mapper0(100) == 100)
  const seedToLocation = F.pipe(...maps.map(singleValueMapper));
  return Math.min(...seeds.map(seedToLocation));
}

export const runB = (data) => {
  const {seeds, maps} = parse(data);
  const seedsRanges = normRanges(seeds.inGroupOf(2).map(([v, l]) => [v, v + l]));
  const rangeMapsSolver = F.pipe(...maps.map(map => map.ranges).map(fillRangesMap).map(rangeMapper));
  
  return rangeMapsSolver(seedsRanges)[0][0]
}

const normRanges = (ranges) => ranges
  .sort((a,b) => a[0] - b[0])
  .reduce((agg, r) => {
    if (agg.at(-1)?.[1] == r[0]) {
      agg.at(-1)[1] = r[1];
    }
    else {
      agg.push(r);
    }
    return agg;
  }, []);

const rangeMapper = (rangeMap) => (valueRanges) => {
  let rMapIdx = 0;
  let rMap = rangeMap[rMapIdx];
  const out = [];
  // console.log('\n\n# RMAP', rangeMap);
  // console.log('# IN', valueRanges);
  valueRanges.reverse();
  while (valueRanges.length > 0) {
    const vr = valueRanges.pop();
    if (vr[0] < rMap?.start) {
      if (vr[1] < rMap.start) {
        out.push(vr);
      } else {
        out.push([vr[0], rMap.start]);
        valueRanges.push([rMap.start, vr[1]]);
      }
    } else if(vr[0] < rMap?.end) {
      if (vr[1] < rMap.end) {
        out.push([vr[0] + rMap.diff, vr[1] + rMap.diff]);
      } else {
        out.push([vr[0] + rMap.diff, rMap.end + rMap.diff]);
        valueRanges.push([rMap.end, vr[1]])
        rMap = rangeMap[++rMapIdx];
      }
    } else if (rMap) {
      valueRanges.push(vr);
      rMap = rangeMap[++rMapIdx];
    } else {
      out.push(vr);
    }
  }
  // console.log('# OUT ', out);
  // console.log('# NOUT', normRanges(out));
  return normRanges(out);
}

const fillRangesMap = (ranges) => ranges
  .map(({src, dst, len}) => ({
    start: src,
    end: src + len,
    diff: dst - src
  }))
  .sort(({start:s1}, {start:s2}) => s1 - s2)
  .reduce((agg, r) => {
    if (agg.length > 0 && agg.at(-1).end !== r.start) {
      agg.push({start: agg.at(-1).end, end: r.start, diff: 0})
    }
    agg.push(r);
    return agg;
  }, [])
  
const singleValueMapper = ({ranges}) => (value) => {
  let range;
  for (let r of ranges) {
    if (value >= r.src) {
      range = r;
    } else {
      break;
    }
  }
  if (range && value < range.src + range.len) {
    return value - range.src + range.dst;
  }
  else {
    return value;
  }
}

const parse = (str) => {
  const [seedsStr, ...mapsStr] = str.split('\n\n')
  return {
    seeds: seedsStr.split(/:\s+/)[1].split(/\s+/).map(S.asInt),
    maps: mapsStr.map(parseMap)
  }
}
const parseMap = (mapStr) => {
  const [header, ...valsStr] = mapStr.split('\n')

  return {
    header, 
    ranges: valsStr.map(s => {
      const [dst, src, len] = s.split(/\s+/).map(S.asInt);
      return {src, dst, len}
    }).sort((a, b) => a.src - b.src)
  }
}
