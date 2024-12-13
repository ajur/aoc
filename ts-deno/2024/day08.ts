import { Grid, Vector, Vec, tc, log, lognb } from "#lib";
// %%
const sample = `
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............`;
const data = await Deno.readTextFile('./data/day08.txt');

const parse = (s: string) => Grid.fromString(s.trim(), c => c)

const printGrid = (g: Grid<string>, ans: Vec[] = []) => {
  const ansSet = new Set(ans.map(an => an.toString()));
  return g.pprint((c, v) => ansSet.has(v.toString()) ? tc.bgBrightRed(c) : c);
}

lognb(printGrid(parse(sample), [[2,2]]))

// %%

const antenasTypes = (g: Grid<string>): Set<string> => new Set(g.findAll(c => c !== '.'))
antenasTypes(parse(sample))

// %%

const findAllAntenas = (g: Grid<string>) => (t: string) => [...g.findAllIndexes((c) => c === t)];
findAllAntenas(parse(sample))('A');
// %%

const getAntinodes = (g: Grid<string>) => ([a, b]: Vector[]): Vector[] => {
  const dv = a.sub(b)
  return [a.add(dv), b.sub(dv)].filter((v) => g.get(v) !== undefined);
}
{
  const g = parse(sample);
  const ans = getAntinodes(g)([new Vector(8, 1), new Vector(5, 2)]);
  lognb(ans);
  lognb(printGrid(g, ans));
}

// %%

const solve = (s: string, antinodesGetter: typeof getAntinodes) => {
  const g = parse(s);
  return antenasTypes(g)
    .values()
    .map(findAllAntenas(g))
    .filter(as => as.length >= 2)
    .flatMap(vs => vs.combinations(2))
    .flatMap(antinodesGetter(g))
    .callOnMe(g => [...g])
    .unique((a, b) => a.eq(b))
    .peekMe(v => {
      log(printGrid(g, v));
    })
    .length;
}
solve(sample, getAntinodes)

// %%
console.log('Sol A:', solve(data, getAntinodes))

// %%

const getAllAntinodes = (g: Grid<string>) => ([a, b]: Vector[]): Vector[] => {
  const dv = a.sub(b)
  const out = [a, b];
  for (let i = 1;; ++i) {
    const v = a.add(dv.mult(i));
    if (g.get(v) === undefined) break;
    out.push(v);
  }
  for (let i = 1;; ++i) {
    const v = b.add(dv.mult(-i));
    if (g.get(v) === undefined) break;
    out.push(v);
  }
  return out;
}
{
  const g = parse(sample);
  const ans = getAllAntinodes(g)([new Vector(8, 8), new Vector(9, 9)]);
  lognb(ans);
  lognb(printGrid(g, ans));
}
solve(sample, getAllAntinodes)
// %%

console.log('Sol B:', solve(data, getAllAntinodes));
