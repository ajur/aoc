// %%

import { and, aStar, fst, gt, key, lt, mapped } from "#lib";

// %%

const sample = `
The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.`

const data = `
The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.`;

const floors = ['first', 'second', 'third', 'fourth'];

type ItemType = "generator" | "microchip";
type State = { elevator: number } & Record<string, number>;
type KindMap = Record<string, { type: ItemType, kind: string }>;
type StateOrTypes = State | [State, KindMap];

const isState = (st: StateOrTypes): st is State => Object.hasOwn(st, 'elevator');
const asState = (st: StateOrTypes): State => isState(st) ? st : st[0];

const parseLine = ([st, km]: [State, KindMap], s: string): [State, KindMap] => {
  const floor = floors.indexOf(s.match(/(\w+) (floor)/)?.[1]!);
  const partsMatch = s.matchAll(/(\w+)(?:-\w+)? (generator|microchip)/g);
  for (const match of partsMatch) {
    const k = `${match[2][0].toUpperCase()}${match[1].substring(0, 3)}`
    km[k] = { type: match[2] as ItemType, kind: match[1] };
    st[k] = floor;
  }
  return [st, km];
}

const parse = (s: string): [State, KindMap] => s.trim()
  .split('\n')
  .reduce(parseLine, [{ elevator: 0 }, {}] as [State, KindMap]);
parse(sample)

// %%
const serializeState = (st: StateOrTypes): string =>
  `E${asState(st).elevator}|` + Object.entries(asState(st))
    .filter(([k]) => k !== 'types' && k !== 'elevator')
    .sort(mapped(fst, (a: string, b: string) => a < b ? -1 : a > b ? 1 : 0))
    .map(([s, v]) => `${s}:${v}`)
    .join('|')

serializeState(parse(sample))
// %%

type Move = {
  from: number;
  to: number;
  items: string[];
}

const doMove = ({to, items}: Move, st: State): State => {
  const nst = { ...st };
  for (const item of items) {
    nst[item] = to;
  }
  nst.elevator = to;
  return nst;
}

serializeState(doMove({from: 0, to: 1, items: ["Mhyd"]}, parse(sample)[0]))

// %%

const getFloorItems = (floor: number, state: State): string[] =>
  Object.entries(state).filter(([k, v]) => k !== "elevator" && k !== "types" && v === floor).map(fst) as string[];

const findAllMoves = (st: StateOrTypes): Move[] => {
  const from = asState(st).elevator;
  const floorItems = getFloorItems(from, asState(st));

  const tos = [from - 1, from + 1].filter(and(gt(-1), lt(floors.length)))
  const combs = [
    ...floorItems.combinations(1),
    ...(floorItems.length > 1 ? floorItems.combinations(2) : [])
  ] as string[][];

  return tos.flatMap(to => combs.map(items => ({ from, to, items })));
}

findAllMoves(parse(sample))
// %%

const stateValidator = (km: KindMap) => (st: State) => {
  for (let floor = 0; floor < floors.length; floor++) {
    const items = getFloorItems(floor, st).map((s => km[s]))
    if (items.length === 0) continue;
    const microchips = items.filter(({ type }) => type === 'microchip').map(key('kind'));
    const generators = items.filter(({ type }) => type === 'generator').map(key('kind'));
    if (generators.length === 0 || microchips.length === 0) continue;
    for (const kind of microchips) {
      if (generators.indexOf(kind) < 0) return false;
    }
  }
  return true;
}

const solve = (s: string): [number, State[]] => {

  const [initialState, km] = parse(s);
  const isValidState = stateValidator(km);

  return aStar({
    start: initialState,
    isEnd: (st: State) => Object.values(st).every(v => v === floors.length - 1),
    // end: Object.entries(initialState).map(([k, _v]) => [k, floors.length - 1]).callOnMe(Object.fromEntries) as State,
    nbs: (s: State): State[] => findAllMoves(s).map((mv) => doMove(mv, s)).filter(isValidState),
    hash: serializeState
  })[0];
}
solve(sample)

// %%

console.log("Sol A:", fst(solve(data)));

// %%

const dataB = `
The first floor contains an elerium generator, an elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip, a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.`;

console.log("Sol B:", fst(solve(dataB)));
