// %%
import { log, Grid, repeat, lognb, HashMap } from "#lib";
import { assertEquals } from "@std/assert";

// %%

/*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+

*/

const numericKeypad = `
789
456
123
.0A`;
const directionalKeypad = `
.^A
<v>`;

const sample = `
029A
980A
179A
456A
379A`
// sol a for sample = 126384

const parse = (s: string) => s.trim().split('\n');

// %%
type MultiWayKeyPadMap = Map<string, Map<string, string[][]>>;
type KeyPadMap = Map<string, Map<string, string[]>>;

const genKeyDirMap = (keypad: string): MultiWayKeyPadMap => {
  const g = Grid.fromString(keypad.trim(), s => s);
  const keyMovesMap = new Map<string, Map<string, string[][]>>();

  g.forEach((k, v) => {
    if (k === '.') return;
    const kg = keyMovesMap.setdefault(k!, new Map<string, string[][]>());
    g.forEach((k2, v2) => {
      if (k2 === '.') return;
      const diff = v2.sub(v);
      const k2m = kg.setdefault(k2!, []);
      if (diff.x === 0 || diff.y === 0) {
        let m: string[] = [];
        if (diff.x > 0) m = repeat('>', diff.x);
        if (diff.x < 0) m = repeat('<', -diff.x);
        if (diff.y > 0) m = repeat('v', diff.y);
        if (diff.y < 0) m = repeat('^', -diff.y);
        m.push('A');
        k2m.push(m);
      } else {
        const mh = (diff.x > 0) ? repeat('>', diff.x) : repeat('<', -diff.x);
        const mv = diff.y > 0 ? repeat('v', diff.y) : repeat('^', -diff.y);
        if (g.get(v.add([diff.x, 0])) !== '.') k2m.push([...mh, ...mv, 'A']);
        if (g.get(v.add([0, diff.y])) !== '.') k2m.push([...mv, ...mh, 'A']);
      }
    })
  })
  return keyMovesMap;
}
const multiToSimpleKeyPad = (kp: MultiWayKeyPadMap): KeyPadMap =>
  new Map(kp.entries().map(([kf, tm]) => [
    kf,
    new Map(tm.entries().map(([tmk, stl]) => [
      tmk,
      stl.sort((l1, l2) => l1.length - l2.length)[0]
    ]))
  ]));

const numKeyMap = genKeyDirMap(numericKeypad);
lognb('num 3 -> 7', numKeyMap.get('3')!.get('7'))
const dirKeyMap = multiToSimpleKeyPad(genKeyDirMap(directionalKeypad));
lognb('dir A', dirKeyMap.get('A'))

// %%

const expandSteps = (steps: string[], keypad: KeyPadMap) => steps.reduce((resp: string[], curr, idx) => [
  ...resp,
  ...keypad.get(idx === 0 ? 'A' : steps[idx - 1])!.get(curr)!
], []);
expandSteps("<<^A".split(''), dirKeyMap)
// %%
/*
v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A
<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A


<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
  v <<   A >>  ^ A   <   A > A  v  A   <  ^ AA > A   < v  AAA >  ^ A
         <       A       ^   A     >        ^^   A        vvv      A
                 0           2                   9                 A
*/
const combineKeyPadMaps = (target: MultiWayKeyPadMap, source: KeyPadMap): MultiWayKeyPadMap =>
  new Map(target.entries().map(([fromKey, toKeyMap]) => [
    fromKey,
    new Map(toKeyMap.entries().map(([toKey, toKeySteps]) => [
      toKey,
      toKeySteps.map(subSteps => expandSteps(subSteps, source))
    ]))
  ]));
combineKeyPadMaps(numKeyMap, dirKeyMap).get('3')!.get('7');

// %%

[dirKeyMap, dirKeyMap].reduce(combineKeyPadMaps, numKeyMap).get('3')!.get('7');

// %%
multiToSimpleKeyPad([dirKeyMap, dirKeyMap].reduce(combineKeyPadMaps, numKeyMap)).get('3')!.get('7');

// %%

const solveA = (s: string) => {
  const targetKeypad = multiToSimpleKeyPad([dirKeyMap, dirKeyMap].reduce(combineKeyPadMaps, numKeyMap));
  return parse(s).map(s => {
    const steps = expandSteps(s.split(''), targetKeypad);
    const num = s.substring(0, s.length - 1).toInt();
    log(s, num, steps.length, steps.join(''))
    return num * steps.length;
  }).sum();
}
assertEquals(solveA(sample), 126384)

// %%
const data = `
340A
149A
582A
780A
463A`

console.log('Sol A:', solveA(data));

// %%

// wont work for bigger Ns
const solveBNaive = (codes: string[], N: number, verbose = false) => {
  const targetKeypad = multiToSimpleKeyPad(repeat(dirKeyMap, N).reduce(combineKeyPadMaps, numKeyMap));
  return codes.map(s => {
    const steps = expandSteps(s.split(''), targetKeypad);
    const num = s.substring(0, s.length - 1).toInt();
    verbose && log(s, num, steps.length, steps.join(''))
    return num * steps.length;
  }).sum();
}
solveBNaive(parse(sample).slice(0, 1), 3, true)

// %%

// checking when we are sure whitch paths are best
// %%
{
  const dirs = [dirKeyMap, dirKeyMap, dirKeyMap, dirKeyMap];
  for (let i = 1; i <= dirs.length; ++i) {
    const eqs = dirs
      .slice(0, i)
      .reduce(combineKeyPadMaps, numKeyMap)
      .values()
      .flatMap(m => m.values())
      .toArray()
      .filter(a => a.length > 1)
      .map(([a1, a2]) => (a1.length < a2.length) ? '<' : ((a1.length > a2.length) ? '>' : '='))
      .join('');
    lognb(`${i} dir key maps -> ${eqs}`)
  }
}
// %%
/*
above outputs
1 dir key maps -> ================================================
2 dir key maps -> >>>><><>><<<<<==>><=<>><<<<<====<=<=><<<<<===<<<
3 dir key maps -> >>>><><>><<<<<<<>><<<>><<<<<<<<<<<<<><<<<<<<<<<<
4 dir key maps -> >>>><><>><<<<<<<>><<<>><<<<<<<<<<<<<><<<<<<<<<<<

Thus with 3 directional keypads we know whitch numeric keypads paths are optimal.
Lets pick them stright away.
*/
const combinedWithOptimalPathsDecided = [dirKeyMap, dirKeyMap, dirKeyMap].reduce(combineKeyPadMaps, numKeyMap);
const optimalNumKeyPad: KeyPadMap = new Map(numKeyMap.entries().map(([fromKey, toKeyMap]) => [
  fromKey,
  new Map(toKeyMap.entries().map(([toKey, toKeySteps]) => {
    if (toKeySteps.length === 1) return [toKey, toKeySteps[0]];
    const [comb0, comb1] = combinedWithOptimalPathsDecided.get(fromKey)!.get(toKey)!;
    return [toKey, toKeySteps[comb0.length < comb1.length ? 0 : 1]];
  }))
]));
optimalNumKeyPad;

// %%
// now we need to know, how much steps add each additional numPad
[optimalNumKeyPad, dirKeyMap, dirKeyMap].reduce(expandSteps, '029A'.split('')).length
// %%

const getCodeLen = (s: string, N: number): number => {
  // const start = [optimalNumKeyPad, dirKeyMap].reduce(expandSteps, s.split(''));
  const start = expandSteps(s.split(''), optimalNumKeyPad);
  const memo = new HashMap<[string, string, number], number>(null, ([a, b, c]) => a + b + c)
  const getLen = (prev: string, next: string, depth: number): number => {
    const path = dirKeyMap.get(prev)!.get(next)!;
    const m = memo.get([prev, next, depth]);
    if (m !== undefined) return m;
    let out = 0;
    if (depth === 0) {
      out = path.length;
    } else {
      out = path.reduce((len, cur, idx) => len + getLen(idx === 0 ? 'A' : path[idx - 1], cur, depth - 1), 0);
    }
    memo.set([prev, next, depth], out);
    return out;
  }
  return start.reduce((len, cur, idx) => len + getLen(idx === 0 ? 'A' : start[idx - 1], cur, N - 1), 0);
}
getCodeLen('029A', 2)

const solveAll = (s: string, N: number) => parse(s)
  .map(code => {
    const num = code.substring(0, code.length - 1).toInt();
    const len = getCodeLen(code, N);
    return num * len;
  }).sum();
assertEquals(solveAll(sample, 2), 126384);
assertEquals(solveAll(data, 6), solveBNaive(parse(data), 6));

// %%
// 25 -> 223335894296628 too high,
// 24 -> 89220508919146 too low -_-
console.log('Sol B:', solveAll(data, 25));
