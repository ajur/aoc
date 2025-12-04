import { asInt, lognb, Grid, Vec } from '#lib';

// %%

const sample = `
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
`;

type ParsedData = Grid<number>;
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => Grid.fromString(s.trim(), c => c === '@' ? 1 : 0);

lognb(parse(sample).pprint())

// %%

const solveA = (grid: ParsedData) => grid
  .map((val, vec, g) =>
    val === 0
      ? 9
      : g.neighbours(vec).reduce((s, [_, v]) => s + (v ?? 0), 0))
  // .peekMe(g => lognb(g.pprint()))
  .count((v) => (v ?? 9) < 4)

solveA(parse(sample));

// %%

const data = await Deno.readTextFile("./data/day04.txt");

console.log("Sol A:", solveA(parse(data)))

// %%

const solveB = (grid: ParsedData) => {
  let removed = 0;
  const removable: Vec[] = [];
  while (true) {
    grid.forEach((v, vec) => {
      if (v === 1) {
        const nbs = grid.neighbours(vec).count(([_, val]) => (val ?? 0) > 0)
        if (nbs < 4) {
          removable.push(vec);
        }
      }
    });
    if (removable.length > 0) {
      while (removable.length > 0) {
        grid.set(removable.pop()!, 0);
        ++removed;
      }
    } else {
      return removed;
    }
  }
}

solveB(parse(sample));
// %%

console.log("Sol B:", solveB(parse(data)))
