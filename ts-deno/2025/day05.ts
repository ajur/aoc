import { lognb, asInt, Interval } from '#lib';
// %%

const sample = `
3-5
10-14
16-20
12-18

1
5
8
11
17
32
`;

type ParsedData = {
  ranges: Interval[],
  ids: number[],
};
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => {
  const [rangesStr, idsStr] = s.trim().split('\n\n');
  return {
    ranges: rangesStr.split('\n').map(Interval.from),
    ids: idsStr.split('\n').map(asInt)
  };
};

lognb(parse(sample))

// %%

const solveA = ({ranges, ids}: ParsedData) => ids.filter(id => ranges.some(r => r.includes(id))).count()

solveA(parse(sample));

// %%

const data = await Deno.readTextFile("./data/day05.txt");

console.log("Sol A:", solveA(parse(data)))

// %%

const solveB = ({ranges}: ParsedData) =>
  Interval.from(ranges).countInts();

solveB(parse(sample));
// %%

console.log("Sol B:", solveB(parse(data)))
