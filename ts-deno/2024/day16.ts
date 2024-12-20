// %%
import { Grid, lognb, log, Vector, Direction, fmtt, aStar, $hash, isVerbose, display, updateDisplay } from "#lib";
import { assertEquals } from "@std/assert";

// %%

const sample1 = `
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############`

const sample2 = `
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################`

const data = await Deno.readTextFile('./data/day16.txt');


const parse = (s: string) => {
  const g = Grid.fromString(s.trim(), c => c);
  const st = g.indexOf('S')!;
  const ed = g.indexOf('E')!;
  g.set(st, '.');
  g.set(ed, '.');
  const dir = Direction.E;
  return { maze: g, end: ed, start: st, dir };
}

{
  const {maze, end, start, dir} = parse(sample1);
  lognb(maze.pprint((c, v) => v.eq(end) ? fmtt('§{red}E') : v.eq(start) ? fmtt('§blue', dir.char) : fmtt('§blackHL' ,c!)))
}
// %%

type Node = [pos: Vector, dir: Direction];

const solveA = (s: string, print = false) => {
  const {maze, end, start, dir} = parse(s);

  const [[cost, path]] = aStar<Node>({
    start: [start, dir],
    isEnd: ([st]) => st.eq(end),
    nbsDists: ([pos, dir]) => [dir.fw(), dir.left(), dir.right()]
      .map(d => [[pos.add(d), d], dir.eq(d) ? 1 : 1001] as [Node, number])
      .filter(([[p]]) => maze.get(p) === '.'),
    hash: ([p, d]: Node) => p[$hash]() + d.name,
  });
  if (print) {
    for (const [p, d] of path) {
      maze.set(p, d.char);
    }
    log(maze.pprint((c, v) => v.eq(start) ? fmtt('§blue', c) : v.eq(end) ? fmtt('§red', c) : fmtt(c !== '.' && c !== '#' ? '§yellow' : '', c!)))
  }
  return cost;
}

assertEquals(solveA(sample1, true), 7036);
assertEquals(solveA(sample2, true), 11048);

// %%
console.log('Sol A:', solveA(data, isVerbose))

// %%

const solveB = (s: string, print = false) => {
  const {maze, end, start, dir} = parse(s);

  const solutions = aStar<Node>({
    start: [start, dir],
    isEnd: ([st]) => st.eq(end),
    nbsDists: ([pos, dir]) => [dir.fw(), dir.left(), dir.right()]
      .map(d => [[pos.add(d), d], dir.eq(d) ? 1 : 1001] as [Node, number])
      .filter(([[p]]) => maze.get(p) === '.'),
    hash: ([p, d]: Node) => p[$hash]() + d.name,
    searchMode: "allBest",
  });
  print && log('solutions', solutions.length)
  for (const [_cost, path] of solutions) {
    for (const [p, _d] of path) {
      maze.set(p, 'O');
    }
  }
  print && log(maze.pprint(c => c === 'O' ? fmtt('§{yellow}O') : c!))

  return maze.findAll(c => c === 'O').toArray().length;
}
assertEquals(solveB(sample1, true), 45);
assertEquals(solveB(sample2, true), 64);
// %%

console.log('Sol B:', solveB(data, isVerbose))
