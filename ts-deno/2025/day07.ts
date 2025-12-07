import { Direction, lognb, Grid } from '#lib';
// %%

const sample =`
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
`;

type ParsedData = Grid<string>;
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => Grid.fromString(s.trim(), c => c)

lognb(parse(sample).pprint())

// %%

const solveA = (g: ParsedData) => {
  let beams = [g.findIndex(c => c === 'S')!];
  let splits = 0;
  while (beams.length > 0 && beams[0][1] < g.rows - 1) {
    const nb = [];
    for (const b of beams) {
      const b2 = b.add(Direction.S);
      const b2v = g.at(b2);
      if (b2v === '.') {
        g.set(b2, '|');
        nb.push(b2);
      } else if (b2v === '^') {
        ++splits;
        for (const bb of [b2.add(Direction.E), b2.add(Direction.W)]) {
          if (g.at(bb)) {
            g.set(bb, '|');
            nb.push(bb);
          }
        }
      }
    }
    beams = nb;
  }
  // lognb(g.pprint());
  return splits;
}

solveA(parse(sample));

// %%

const data = await Deno.readTextFile("./data/day07.txt");

console.log("Sol A:", solveA(parse(data)))

// %%

const solveB = (g: ParsedData) => {
  const gn = g.map(() => 1);
  for (let r = g.rows - 2; r >= 0; --r) {
    for (let c = 0; c < g.cols; ++c) {
      switch(g.at(c, r)) {
        case '.':
          gn.set([c, r], gn.at(c, r + 1)!);
          break;
        case '^':
          gn.set([c, r], gn.at(c - 1, r + 1)! + gn.at(c + 1, r + 1)!);
          break;
        case 'S':
          return gn.at(c, r + 1)!;
      }
    }
  }
  return -1;
}

lognb(solveB(parse(sample)))
// %%

console.log("Sol B:", solveB(parse(data)))
