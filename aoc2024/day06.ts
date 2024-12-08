import { Grid, VecTuple, Vector, asInt, timeout, ulog } from  './lib/index.ts';
// %%

const sample = `
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...`

enum Field {
  FLOOR        =       0b0,
  OBSTACLE     =       0b1,
  NEW_OBSTACLE =      0b11,
  CURRENT      =     0b100,
  UP           =    0b1000,
  DOWN         =   0b10000,
  LEFT         =  0b100000,
  RIGHT        = 0b1000000,
  PATH         = 0b1111000,
}

const PARSE_MAP: Record<string, Field> = {
  '.': Field.FLOOR,
  '#': Field.OBSTACLE,
  '^': Field.CURRENT,
}

const drawField = (f: number | undefined, pos: Vector, curPos?: Vector) => {
  if (curPos?.eq(pos)) return '@';
  switch(f) {
    case Field.FLOOR: return '.';
    case Field.OBSTACLE: return '#';
    case Field.NEW_OBSTACLE: return '\u001b[31mO\u001b[39m';
    case Field.CURRENT: return '\u001b[31m@\u001b[39m';
    case Field.UP: return '^';
    case Field.DOWN: return 'v';
    case Field.LEFT: return '<';
    case Field.RIGHT: return '>';
    case (Field.UP & Field.DOWN): return '|';
    case (Field.LEFT & Field.RIGHT): return '-';
    default: return '+';
  }
}

const dirToVal = (v: Vector): Field => {
  if (v.eq([0, -1])) return Field.UP;
  if (v.eq([0, 1])) return Field.DOWN;
  if (v.eq([-1, 0])) return Field.LEFT;
  if (v.eq([1, 0])) return Field.RIGHT;
  return 0;
}

const parse = (s: string): Grid<number> => Grid.fromString(s.trim(), (v) => PARSE_MAP[v])
// console.log(parse(sample).pprint((val, pos) => drawField(val, pos)))


// %%
const walk = async (g: Grid<number>, animate = 0) => {
  let guardPos = g.indexOf(Field.CURRENT)!;
  let dir = new Vector(0, -1);

  let logger: any;
  try {
   logger = animate ? await ulog("walker", g.pprint()) : null;
  } catch(_) {
    animate = 0;
  }
  while (g.get(guardPos) !== undefined) {
    g.set(guardPos, g.get(guardPos)! | dirToVal(dir));
    const nPos = guardPos.add(dir);
    const val = g.get(nPos);
    if (val !== undefined && (val & Field.OBSTACLE)) {
      dir = dir.rotate(Math.PI / 2, true);
      continue;
    } else {
      guardPos = nPos;
    }
    if (animate) {
      await timeout(animate);
      await logger!(g.pprint((val, pos) => drawField(val, pos, guardPos)))
    }
  }
  return g;
}

await walk(parse(sample), 50);
// %%

const solveA = async (s: string) => (await walk(parse(s))).count(v => !!(v! & Field.PATH));
await solveA(sample)
// %%
const data = await Deno.readTextFile('./data/day06.txt');
console.log("Sol A:", await solveA(data))

// %%

const walkLoopCheck = (grid: Grid<number>, newObstacle?: Vector, tryAltPaths = false, log = false): [Grid<number>, hasLooped: boolean, Set<string>] => {
  const g = grid.clone();
  if (newObstacle) {
    g.set(newObstacle, Field.NEW_OBSTACLE);
  }
  const startPos = grid.indexOf(Field.CURRENT)!;
  let guardPos = startPos;
  let dir = new Vector(0, -1);
  let stepsMade = 0;
  const possibleObstacles = new Set<string>();

  const logger = log
    ? (steps: number, obstacles: number) => {
      // console.clear();
      console.log(`steps ${steps.toString().padStart(6)}, possible obstacles found ${obstacles.toString().padStart(4)}`);
    }
    : () => {};

  while (g.get(guardPos) !== undefined) {
    const nPos = guardPos.add(dir);
    const nVal = g.get(nPos) ?? Field.FLOOR;

    if (nVal & Field.OBSTACLE) {
      dir = dir.rotate(Math.PI / 2, true);
      continue;
    } else {
      if (tryAltPaths && !nPos.eq(startPos) && !possibleObstacles.has(nPos.toString())) {
        const [_, altGLooped] = walkLoopCheck(grid, nPos);
        if (altGLooped) {
          possibleObstacles.add(nPos.toString());
        }
      }
      const dirVal = dirToVal(dir);
      if (g.get(guardPos)! & dirVal) return [g, true, possibleObstacles];
      g.set(guardPos, (g.get(guardPos)! | dirToVal(dir)));
      guardPos = nPos;
      ++stepsMade;
    }
    // logger(stepsMade, possibleObstacles.length);
  }
  return [g, false, possibleObstacles];
}

{
  const g = parse(data);
  // g.set([3, 6], OBSTACLE);
  const [_, __, possibleObstacles] = walkLoopCheck(g, undefined, true, true);
  possibleObstacles.forEach((v) => g.set(v.split(',').map(asInt) as VecTuple, Field.NEW_OBSTACLE));
  // console.log(g.pprint((val, pos) => drawField(val, pos)));
  possibleObstacles.size
}
// %%

const solveB = (s: string) => {
  const g = parse(s);
  const [_, __, possibleObstacles] = walkLoopCheck(g, undefined, true);
  return possibleObstacles.size;
}
solveB(sample)

// %%

console.log('Sol B:', solveB(data))
