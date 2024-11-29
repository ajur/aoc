
import { manhattan } from  './lib/index.ts';
// %%

type Turn = 'L' | 'R' | 'F';
type Step = [Turn, number];

const parse = (str: string): Step[] => str.split(', ').map(s => [s[0] as Turn, s.substring(1).toInt()]);

parse("R5, L5, R5, R003")

// %%

enum Dir { N = 0, E, S, W };
type State = { dir: Dir, x: number, y: number};

const walk = ({dir, x, y}: State, [turn, steps]: Step): State => {
  const ndir = turn === 'F' ? dir : Math.mod(dir + (turn === 'L' ? -1 : 1), 4);
  return {
    dir: ndir,
    x: x + (ndir === Dir.W ? -steps : ndir === Dir.E ? steps : 0),
    y: y + (ndir === Dir.S ? -steps : ndir === Dir.N ? steps : 0),
  }
}

const start = {dir: Dir.N, x: 0, y: 0};
const walkPath = (steps: Step[]): State => steps.reduce(walk, start);

const countDist = (data: string) => manhattan(start, walkPath(parse(data)))

countDist("R5, L5, R5, R003")
// %%
const data = "L4, L1, R4, R1, R1, L3, R5, L5, L2, L3, R2, R1, L4, R5, R4, L2, R1, R3, L5, R1, L3, L2, R5, L4, L5, R1, R2, L1, R5, L3, R2, R2, L1, R5, R2, L1, L1, R2, L1, R1, L2, L2, R4, R3, R2, L3, L188, L3, R2, R54, R1, R1, L2, L4, L3, L2, R3, L1, L1, R3, R5, L1, R5, L1, L1, R2, R4, R4, L5, L4, L1, R2, R4, R5, L2, L3, R5, L5, R1, R5, L2, R4, L2, L1, R4, R3, R4, L4, R3, L4, R78, R2, L3, R188, R2, R3, L2, R2, R3, R1, R5, R1, L1, L1, R4, R2, R1, R5, L1, R4, L4, R2, R5, L2, L5, R4, L3, L2, R1, R1, L5, L4, R1, L5, L1, L5, L1, L4, L3, L5, R4, R5, R2, L5, R5, R5, R4, R2, L1, L2, R3, R5, R5, R5, L2, L1, R4, R3, R1, L4, L2, L3, R2, L3, L5, L2, L2, L1, L2, R5, L2, L2, L3, L1, R1, L4, R2, L4, R3, R5, R3, R4, R1, R5, L3, L5, L5, L3, L2, L1, R3, L4, R3, R2, L1, R3, R1, L2, R4, L3, L3, L3, L1, L2";

console.log("Sol A:", countDist(data));
// %%

const visit = (alreadyVisited: Record<string, boolean>, {x, y}: State) => {
  const key = `${x},${y}`;
  // console.log(key);
  if (alreadyVisited[key]) {
    // console.log('visited');
    return true;
  }
  else {
    // console.log('not visited');
    alreadyVisited[key] = true;
    return false;
  }
}

const walkTill2ndVisit = (steps: Step[]) => {
  const hasVisited = {};
  let state = start;
  visit(hasVisited, start);
  for (const [dir, dist] of steps) {
    if (dist <= 0) continue;
    state = walk(state, [dir, 1]);
    if (visit(hasVisited, state)) return state;
    for (let i = 1; i < dist; ++i) {
      state = walk(state, ['F', 1]);
      if (visit(hasVisited, state)) return state;
    }
  }
  throw new Error("ups, shouldn't happen");
}

const countDistOn2ndEntry = (data: string) => manhattan(start, walkTill2ndVisit(parse(data)))
countDistOn2ndEntry("R8, R4, R4, R8") === 4;

// %%

console.log("Sol B:", countDistOn2ndEntry(data));
