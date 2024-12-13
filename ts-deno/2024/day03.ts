
import { asInt } from  "#lib";
// %%

const sample = `xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))`;
const regex = /mul\((\d{1,3}),(\d{1,3})\)/g;

[...sample.matchAll(regex)]

// %%

const solveA = (s: string) => s.matchAll(regex).map(([_, a, b]) => asInt(a) * asInt(b)).toArray().sum();
solveA(sample)
// %%

const data = await Deno.readTextFile('./data/day03.txt');

console.log("Sol A:", solveA(data))

// %%

const sampleB = `xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))`;

const regexB = /(?:(mul|do|don't)\((?:(\d{1,3}),(\d{1,3}))?\))/g;
[...sampleB.matchAll(regexB)]

// %%

type Op = [string, number, number];
type Resp = [boolean, number];

const solveB = (s: string) => s.matchAll(regexB)
  .map(([_, op, a, b]) => [op, asInt(a??'0'), asInt(b??'0')] as Op)
  .reduce(([isOn, num]: Resp, [op, a, b]: Op): Resp => {
    if (op === "mul" && isOn) {
      return [isOn, num + a * b];
    } else if (op === "do") {
      return [true, num];
    } else if (op === "don't") {
      return [false, num];
    } else {
      return [isOn, num];
    }
  }, [true, 0])
  .at(1);

solveB(sampleB)
// %%

console.log("Sol B:", solveB(data))
