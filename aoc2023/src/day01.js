
import {strings as S} from './utils.js'

export const sampleA = `
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7ucone
`

export const sampleB = `
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`

export const runA = (data) => parse(data).map((str) => {
  const nums = str.match(/\d/g);
  return S.asInt(nums.at(0) + nums.at(-1));
}).sum();

export const runB = (data) => parse(data).map((str) => {
  const num1 = asDigit(str.match(/(\d|one|two|three|four|five|six|seven|eight|nine)/)[1]);
  const num2 = asDigit(str.match(/.*(\d|one|two|three|four|five|six|seven|eight|nine)/)[1]);
  return S.asInt(num1 + num2);
}).sum();

const parse = (str) => str.split('\n');

const a_to_n = {
  "one": "1",
  "two": "2",
  "three": "3",
  "four": "4",
  "five": "5",
  "six": "6",
  "seven": "7",
  "eight": "8",
  "nine": "9",
}
const asDigit = (s) => a_to_n[s] || s;

