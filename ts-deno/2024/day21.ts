// %%
import { log, Grid, repeat, lognb, compose, peek } from "#lib";
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
type KeyPadMap = Map<string, Map<string, string[][]>>;

const genKeyDirMap = (keypad: string): KeyPadMap => {
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
        const mv = diff.y > 0 ? repeat('v', diff.y) : repeat('^', diff.y);
        if (g.get(v.add([diff.x, 0])) !== '.') k2m.push([...mh, ...mv, 'A']);
        if (g.get(v.add([0, diff.y])) !== '.') k2m.push([...mv, ...mh, 'A']);
      }
    })
  })
  return keyMovesMap;
}

const numKeyMap = genKeyDirMap(numericKeypad);
lognb('num 3 -> 7', numKeyMap.get('3')!.get('7'))
const dirKeyMap = genKeyDirMap(directionalKeypad);
lognb('dir A', dirKeyMap.get('A'))

// %%

const getDirections = (keypad: KeyPadMap) => (code: string[]) => {
  const steps: string[] = [];
  let current = 'A';
  for (const next of code) {
    steps.push(...keypad.get(current)!.get(next)!, 'A')
    current = next;
  }
  return steps;
}
const getNumDirs = getDirections(numKeysSteps);
const getDirDirs = getDirections(dirKeysSteps);
{
  const code = "379A".split('');
  const nd = getNumDirs(code);
  lognb('num kpd dir:', nd.join(''));
  lognb('dir kpd dir:', getDirDirs(nd).join(''));
  const getDirs = compose(getDirDirs, getDirDirs, getNumDirs);
  lognb('final:', getDirs(code).join(''));
}

// %%

const solveA = (s: string) => {
  const getDirs = compose(getDirDirs, getDirDirs, getNumDirs);
  return parse(s).map(s => {
    const steps = getDirs(s.split(''));
    const num = s.substring(0, s.length - 1).toInt();
    console.log(s, num, steps.length, steps.join(''))
    return num * steps.length;
  }).sum();
}
assertEquals(solveA(sample), 126384)
