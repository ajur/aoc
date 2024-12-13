// %%
import { Vector } from "#lib";
// %%

const sample = `
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279`
const data = await Deno.readTextFile('./data/day13.txt')

type Machine = {
  a: Vector,
  b: Vector,
  p: Vector,
}

const machineMatcher = /Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)/;
const parse = (s: string): Machine[] => s.trim().split('\n\n').map(ms => {
  const m = ms.match(machineMatcher);
  if (!m) throw Error("couldnt parse " + ms);
  return {
    a: new Vector(m[1].toInt(), m[2].toInt()),
    b: new Vector(m[3].toInt(), m[4].toInt()),
    p: new Vector(m[5].toInt(), m[6].toInt())
  } as Machine;
});
parse(sample)

// %%

const isPossible = ({a, b, p}: Machine) => p.x % Math.gcd(a.x, b.x) === 0 && p.y % Math.gcd(a.y, b.y) === 0;
parse(sample).filter(isPossible)

// %%

const solveMachine = ({a, b, p}: Machine) => Math.linear2(a.x, b.x, p.x, a.y, b.y, p.y);

parse(sample).filter(isPossible).map(solveMachine)
// %%

const solveA = (s: string) => parse(s)
  .filter(isPossible)
  .map(solveMachine)
  .filter(([a, b]) => Number.isInteger(a) && Number.isInteger(b))
  .map(([a, b]) => a * 3 + b * 1)
  .sum()

solveA(sample)

// %%

console.log('Sol A:', solveA(data));

// %%

const increase = 10000000000000;

const solveB = (s: string) => parse(s)
  .map(({a, b, p}: Machine) => ({a, b, p: p.add(increase)} as Machine))
  .filter(isPossible)
  .map(solveMachine)
  .filter(([a, b]) => Number.isInteger(a) && Number.isInteger(b))
  .map(([a, b]) => a * 3 + b * 1)
  .sum()

solveB(sample)

// %%

console.log('Sol B:', solveB(data));
