import { memo, asInt } from "./lib/index.ts";
// %%

const sample = '125 17';
const data = '64599 31 674832 2659361 1 0 8867 321';

const parse = (s: string) => s.trim().split(' ').map(asInt);

parse(sample)

// %%

const solveA = (s: string, blinks: number) => {
  const stones = parse(s);

  const countAfterBlink = memo((blinksLeft: number, stoneNum: number): number => {
    if (blinksLeft === 0) {
      return 1;
    }
    if (stoneNum === 0) {
      return countAfterBlink(blinksLeft - 1, 1);
    }
    const digits = stoneNum.digits();
    if (digits % 2 === 0) {
      return countAfterBlink(blinksLeft - 1, stoneNum.firstNDigits(digits / 2))
        + countAfterBlink(blinksLeft - 1, stoneNum.lastNDigits(digits / 2));
    }
    return countAfterBlink(blinksLeft - 1, stoneNum * 2024);
  });

  return stones.map(n => countAfterBlink(blinks, n)).sum();
}

solveA(sample, 25)

// %%

console.log('Sol A:', solveA(data, 25));
// %%

console.log('Sol B:', solveA(data, 75));
