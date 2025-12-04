import { asInt, lognb } from '#lib';

// %%

const sample = `
987654321111111
811111111111119
234234234234278
818181911112111
`;

type ParsedData = number[][];
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => s.trim()
  .split('\n')
  .map(l => l.trim().split('').map(asInt));

parse(sample)

// %%

const findFirstMaxInRange = (pack: number[], from: number, to: number): [number, number] => {
    let max = 0;
    let maxIdx = -1;
    for (let i = to - 1; i >= from; --i) {
      if (pack[i] >= max) {
        max = pack[i];
        maxIdx = i;
      }
    }
    return [max, maxIdx];
}

lognb(findFirstMaxInRange([9, 8, 3, 9], 0, 4))
lognb(findFirstMaxInRange([9, 8, 3, 9], 1, 3))
// %%

const maxJoltage = (bateries: number) => (pack: number[]): number => {
  const bat: number[] = [];
  let startIdx = 0;
  for (let b = 0; b < bateries; ++b) {
    const lastIdx = pack.length - bateries + b + 1;
    const [max, maxIdx] = findFirstMaxInRange(pack, startIdx, lastIdx);
    startIdx = maxIdx + 1;
    bat.push(max);
  }
  return asInt(bat.join(''));
}

lognb(maxJoltage(2)([9, 8, 3, 9]))
lognb(maxJoltage(4)([9, 8, 3, 9]))

// %%

const solveA = (clicks: ParsedData) => clicks.map(maxJoltage(2)).sum()

solveA(parse(sample));

// %%

const data = await Deno.readTextFile("./data/day03.txt");

console.log("Sol A:", solveA(parse(data)))

// %%

const solveB = (clicks: ParsedData) => clicks.map(maxJoltage(12)).sum()

solveB(parse(sample));
// %%

console.log("Sol B:", solveB(parse(data)))
