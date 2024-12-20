// %%
import { Grid, lognb, log, Vector, fmtt, aStar, isVerbose, HashMap, isJupyter } from "#lib";
import { assertEquals } from "@std/assert";

// %%

const sample = `
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############`

const data = await Deno.readTextFile('./data/day20.txt');


const parse = (s: string) => {
  const g = Grid.fromString(s.trim(), c => c);
  const st = g.indexOf('S')!;
  const ed = g.indexOf('E')!;
  g.set(st, '.');
  g.set(ed, '.');
  return { maze: g, end: ed, start: st };
}

{
  const {maze, end, start} = parse(sample);
  lognb(maze.pprint((c, v) => v.eq(end) ? fmtt('§{red}E') : v.eq(start) ? fmtt('§{blue}S') : fmtt('§blackHL' ,c!)))
}
// %%

type Shortcut = [from: Vector, through: Vector, to: Vector];

const printPath = (g: Grid<string>, trail: Vector[], shortcuts: Shortcut[] = []) => {
  const ansSet = new HashMap<Vector, boolean>(trail.map(v => [v,true]));
  const scSet = new HashMap<Vector, boolean>(shortcuts.flat().map(v => [v, true]))
  return g.pprint((c, v) => fmtt('§blackHL', scSet.has(v) ? '§bgBlue' : '', ansSet.has(v) ? fmtt('§red§O') : c!));
}

const findPath = (maze: Grid<string>, start: Vector, end: Vector): [number, Vector[]] => {
  const resp = aStar({
    start: start,
    isEnd: (v: Vector) => end.eq(v),
    nbs: (v: Vector) => v.orthogonals().filter(nb => maze.get(nb) === '.'),
    hash: (v: Vector) => v.toString(),
    score: (v: Vector) => end.manhattan(v),
  });
  if (resp[0]?.[0] >= 0) return resp[0];
  else return [-1, []];
}
{
  const {maze, start, end} = parse(isJupyter ? sample : data);
  const [stepsNoCheat, pathNoCheat] = findPath(maze, start, end);
  log("no cheat:", stepsNoCheat);
  log(printPath(maze, pathNoCheat))
  // just to be sure:
  for (const step of pathNoCheat) maze.set(step, '0');
  log("Any unesed path tiles?", maze.findAllIndexes(c => c === '.').toArray())
  }

// %%

const shortcutsForPos = (g: Grid<string>, pos: Vector): Shortcut[] => {
  const out: Shortcut[] = [];
  for (const d of (new Vector(0).orthogonals())) {
    const p1 = pos.add(d);
    if (g.get(p1) === '#') {
      const p2 = p1.add(d);
      if (g.get(p2) === '.') {
        out.push([pos, p1, p2]);
      }
    }
  }
  return out;
}
{
  const {maze, start, end} = parse(sample);
  const [_stepsNoCheat, pathNoCheat] = findPath(maze, start, end);
  const cutOnStep = 13;
  const pathTillCut = pathNoCheat.slice(0, cutOnStep)
  const cutOn = pathTillCut.at(-1)!;
  const shc = shortcutsForPos(maze, cutOn)[0];
  const [shcSteps, shcPath] = findPath(maze, shc[2], end);
  const newPath = [...pathTillCut, ...shcPath];
  lognb("with shortcut:", pathTillCut.length + 1 + shcSteps)
  lognb(printPath(maze, newPath, [shc]));
}

// %%

type ShortcutDist = [steps: number, saved: number, Shortcut];

const solveA = (s: string, verbose = false) => {
  const {maze, start, end} = parse(s);
  const findFrom = (st: Vector) => findPath(maze, st, end);

  const [stepsNoCheat, pathNoCheat] = findFrom(start);

  const allSteps = new HashMap<Vector, number>(pathNoCheat.map((v, i) => [v, pathNoCheat.length - i - 1]));
  const stepsChecked = new HashMap<Vector, boolean>();
  const shortcuts: ShortcutDist[] = [];

  for (let i = 0; i < pathNoCheat.length - 2; ++i) {
    const cutOn = pathNoCheat[i];
    const stepsToCutOn = i + 1;
    for (const shc of shortcutsForPos(maze, cutOn)) {
      const [_st, _md, end] = shc;
      if (stepsChecked.has(end)) {
        // skip
      } else if (allSteps.has(end)) {
        const shcSteps = stepsToCutOn + 1 + allSteps.get(end)!;
        shortcuts.push([shcSteps, stepsNoCheat - shcSteps, shc]);
      } else {
        // this is dead code - no such cases in given data... but i'll keep it here
        const [steps] = findFrom(end);
        if (steps >= 0) {
          const shcSteps = stepsToCutOn + 1 + steps;
          if (shcSteps < stepsNoCheat) {
            shortcuts.push([shcSteps, stepsNoCheat - shcSteps, shc]);
          }
        }
      }
    }
    stepsChecked.set(cutOn, true);
  }

  if (verbose) {
    log(printPath(maze, pathNoCheat, shortcuts.map(s => s[2])));
    if (shortcuts.length < 45) { // probably sample data
      const shortcutsByLength = Object.entries(Object.groupBy(shortcuts, ([_, saved]) => saved)).map(([n, a]) => [+n, a?.length ?? 0])
      shortcutsByLength.sort(([n1], [n2]) => n1-n2);
      for (const [n, l] of shortcutsByLength) lognb(n, '->', l);
    }
  }

  return shortcuts.filter(([_, s]) => s >= 100).length;
}
solveA(sample, true)

// %%

console.log('Sol A:', solveA(data))

// %%

const solveB = (s: string, minSaved: number, verbose = false) => {
  const {maze, start, end} = parse(s);
  const findFrom = (st: Vector) => findPath(maze, st, end);

  const [_stepsNoCheat, pathNoCheat] = findFrom(start);

  const cheats = new Map<number, number>();

  for (let i = 0; i < pathNoCheat.length; ++i) {
    const cutOn = pathNoCheat[i];

    for (let j = i + minSaved - 1; j < pathNoCheat.length; ++j) {
      const stepsOnPath = j - i;
      const cheatEnd = pathNoCheat[j];
      const cheatDist = cutOn.manhattan(cheatEnd);
      if (cheatDist > 20) continue;
      const saved = stepsOnPath - cheatDist;
      if (saved >= minSaved) {
        cheats.set(saved, (cheats.get(saved) || 0) + 1);
      }
    }
  }
  if (verbose) {
    const arr = cheats.entries().toArray().sort();
    log(arr.map(([n, c]) => `${n} -> ${c}`).join('\n'))
    if (minSaved === 50) {
      const expected = [[50, 32], [52, 31], [54, 29], [56, 39], [58, 25], [60, 23], [62, 20], [64, 19], [66, 12], [68, 14], [70, 12], [72, 22], [74, 4], [76, 3]];
      assertEquals(arr, expected);
    }
  }
  return cheats.values().toArray().sum();
}
solveB(sample, 50, true);

// %%

console.log("Sol B:", solveB(data, 100));
