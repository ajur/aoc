import { asInt } from "./lib/index.ts";
// %%

const sample = `
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20`
const data = await Deno.readTextFile('./data/day07.txt');

type Equation = { test: number, nums: number[] };

const parse = (s: string): Equation[] => s.trim().split('\n').map(l => {
  const [test, nums] = l.split(': ');
  return {
    test: asInt(test),
    nums: nums.split(' ').map(asInt)
  };
})
parse(sample)
// %%

const testNums = (nums: number[], expected: number): boolean => {
  if (nums.length === 2) {
    return nums[0] + nums[1] === expected || nums[0] * nums[1] === expected;
  }
  const last = nums.at(-1)!;
  const rest = nums.slice(0, nums.length - 1);
  return testNums(rest, expected - last) || (Number.isInteger(expected / last) && testNums(rest, expected / last));
}
parse(sample).filter(({nums, test}: Equation) => testNums(nums, test));
// %%

const solve = (s: string, tester: (nums: number[], expected: number) => boolean) => parse(s)
  .filter(({nums, test}: Equation) => tester(nums, test))
  .map(({test}) => test)
  .sum();
solve(sample, testNums)

// %%

console.log('Sol A:', solve(data, testNums));
// %%

const testNumsB = (nums: number[], expected: number): boolean => {
  if (nums.length === 1)
    return nums[0] === expected;
  if (nums.length === 2) {
    return nums[0] + nums[1] === expected
        || nums[0] * nums[1] === expected
        || nums.join('').toInt() === expected;
  }
  const last = nums.at(-1)!;
  const rest = nums.slice(0, nums.length - 1);
  if(testNumsB(rest, expected - last) || (Number.isInteger(expected / last) && testNumsB(rest, expected / last)))
    return true;
  const lastS = '' + last;
  const exptS = '' + expected;
  if (!exptS.endsWith(lastS)) return false;
  return testNumsB(rest, exptS.substring(0, exptS.length - lastS.length).toInt());
}
parse(sample).filter(({nums, test}: Equation) => testNumsB(nums, test));
// %%

solve(sample, testNumsB)
// %%
console.log('Sol B:', solve(data, testNumsB))
