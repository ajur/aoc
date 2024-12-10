import { Grid, Vector, Vec, tc, log, lognb, asInt, eq, HashMap } from "./lib/index.ts";
// %%

const sample = `
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732`
const data = await Deno.readTextFile('./data/day10.txt');

const parse = (s: string) => Grid.fromString(s.trim(), asInt)

const printTrail = (g: Grid<number>, trail: Vector[]) => {
  const ansSet = new HashMap<Vector, boolean>(trail.map(v => [v,true]));
  return g.pprint((c, v) => ansSet.has(v) ? tc.bgBrightRed(''+c) : ''+c);
}
lognb(printTrail(parse(sample), [new Vector(2, 0), new Vector(3, 0), new Vector(3, 1)]))
// %%

const findTrailsFrom0To9 = (g: Grid<number>) => (v0: Vector): Vector[][] => {
  const found9s = new HashMap<Vector, Vector[]>();
  const goUp = (path: Vector[]): void => {
    const v = path.at(-1)!;
    const val = g.get(v)!;
    if (val === 9 && !found9s.has(v)) {
      found9s.set(v, path);
    } else {
      const nbs = v.orthogonals().filter(nb => g.get(nb) === val + 1);
      for (const nb of nbs) {
        goUp([...path, nb]);
      }
    }
  }
  goUp([v0]);
  return found9s.values().toArray();
}
{
  const g = parse(sample);
  const trails = findTrailsFrom0To9(g)(g.findIndex(eq(0))!);
  trails.forEach(t => lognb(printTrail(g, t)));
  trails.length
}
// %%

const solveA = (s: string) => {
  const g = parse(s);
  return g.findAllIndexes(eq(0))
    .map(findTrailsFrom0To9(g))
    .map(trails => trails.length)
    .toArray().sum();
}
solveA(sample)
// %%

console.log("Sol A:", solveA(data));
// %%

const findAllTrailsFrom0To9 = (g: Grid<number>) => (v0: Vector): Vector[][] => {
  const goUp = (path: Vector[]): Vector[][] => {
    const v = path.at(-1)!;
    const val = g.get(v)!;
    if (val === 9) {
      return [path];
    }
    const nbs = v.orthogonals().filter(nb => g.get(nb) === val + 1);
    return nbs.flatMap(nb => goUp([...path, nb]));
  }
  return goUp([v0]);
}

const solveB = (s: string) => {
  const g = parse(s);
  return g.findAllIndexes(eq(0))
    .map(findAllTrailsFrom0To9(g))
    .map(trails => trails.length)
    .toArray().sum();
}
solveB(sample)
// %%

console.log('Sol B:', solveB(data));
