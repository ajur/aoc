// %%
import { Vector, Grid, log, eq, notNull, display, updateDisplay, timeout } from "#lib";
import { isVerbose } from "../lib/common.ts";
import { isJupyter } from "../lib/common.ts";

// %%

const sample = `
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv
<v>>v<<
`

const dirMap: Record<string, Vector> = {
  '<': new Vector(-1, 0),
  '^': new Vector(0, -1),
  '>': new Vector(1, 0),
  'v': new Vector(0, 1)
};

const parse = (s: string): [Grid<string>, Vector[]] => {
  const [gridString, stepsString] = s.trim().split('\n\n');
  const g = Grid.fromString(gridString, c => c);
  return [g, stepsString.split('').map(c => dirMap[c]).filter(notNull)]
}
{
  const [g, m] = parse(sample)
  log(g.pprint())
  m
}
// %%

const moveIfPossible = (g: Grid<string>, pos: Vector, dir: Vector): Vector | undefined => {
  const np = pos.add(dir);
  if (g.get(np) === 'O') {
    moveIfPossible(g, np, dir);
  }
  if (g.get(np) === '.') {
    g.set(np, g.get(pos)!);
    g.set(pos, '.');
    return np;
  }
  return pos;
}

const moveAll = async (g: Grid<string>, moves: Vector[], algo: typeof moveIfPossible, animate = 0) => {
  let robotPos = g.indexOf('@')!;
  if (!isJupyter && !isVerbose) animate = 0;
  animate && await display(g.pprint());
  for (const move of moves) {
    robotPos = algo(g, robotPos, move)!;
    animate && await timeout(animate)
    animate && await updateDisplay(g.pprint());
  }
  return g;
}

await moveAll(...parse(sample), moveIfPossible, 100) && '';
// %%
const solveA = async (s: string, animate = 0) =>
  (await moveAll(...parse(s), moveIfPossible, animate))
    .findAllIndexes(eq('O'))
    .toArray()
    .reduce((v, [x, y]) => v + x + 100*y, 0)

await solveA(sample) === 2028

// %%

const sample2 = `
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
`

await solveA(sample2, 10) === 10092;

// %%

const data = await Deno.readTextFile('./data/day15.txt');

console.log("Sol A:", await solveA(data));

// %%

const parseB = (s: string): [Grid<string>, Vector[]] => {
  const [gridString, stepsString] = s.trim().split('\n\n');
  const expGridStr = gridString.replaceAll('#', '##').replaceAll('.', '..').replaceAll('@', '@.').replaceAll('O', '[]');
  const g = Grid.fromString(expGridStr, c => c);
  return [g, stepsString.split('').map(c => dirMap[c]).filter(notNull)]
}
log(parseB(sample)[0].pprint())
// %%

const canMoveVert = (g: Grid<string>, pos: Vector, dir: Vector): boolean => {
  const cval = g.get(pos)!;
  const npos = pos.add(dir);
  if (cval === '#') return false;
  if (cval === '[' || cval === ']') {
    return canMoveVert(g, npos, dir) && canMoveVert(g, npos.add([(cval === '[' ? 1 : -1),0]), dir);
  }
  return true;
}
const moveVert = (g: Grid<string>, pos: Vector, dir: Vector) => {
  const cval = g.get(pos)!;
  const npos = pos.add(dir);
  if (cval === '@' || cval === '[' || cval === ']') {
    moveVert(g, npos, dir);
    const posAlt = cval === '@' ? null : pos.add([(cval === '[' ? 1 : -1),0]);
    if (posAlt) moveVert(g, posAlt.add(dir), dir);

    g.set(npos, cval);
    g.set(pos, '.');
    if (posAlt) {
      g.set(posAlt.add(dir), g.get(posAlt)!);
      g.set(posAlt, '.');
    }
  }
}

const wideMoveIfPossible = (g: Grid<string>, pos: Vector, dir: Vector): Vector | undefined => {
  const np = pos.add(dir);
  if (g.get(np) === '.') {
    g.set(np, g.get(pos)!);
    g.set(pos, '.');
    return np;
  }
  if (dir.y === 0 && (g.get(np) === '[' || g.get(np) === ']')) {
    wideMoveIfPossible(g, np, dir);
    if (g.get(np) === '.') {
      g.set(np, g.get(pos)!);
      g.set(pos, '.');
      return np;
    }
  } else if(dir.x === 0 && (g.get(np) === '[' || g.get(np) === ']')) {
    if (canMoveVert(g, np, dir)) {
      moveVert(g, pos, dir)
      return np;
    }
  }
  return pos;
}

await moveAll(...parseB(sample), wideMoveIfPossible, 100)

// %%

const solveB = async (s: string, animate = 0) =>
  (await moveAll(...parseB(s), wideMoveIfPossible, animate))
  .findAllIndexes(eq('['))
  .toArray()
  .reduce((v, [x, y]) => v + x + 100*y, 0)

{
  await solveB(sample2, 10) === 9021;
}

// %%

console.log('Sol B:', await solveB(data));
