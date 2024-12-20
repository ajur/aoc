// %%
import { log, ansi, lognb } from "#lib";
import { assertEquals } from "@std/assert";
// %%

const sample = `
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb`;
const data = await Deno.readTextFile('./data/day19.txt');

type Parsed = [towels: string[], patterns: string[]];

const parse = (s: string): Parsed => {
  const [lt, lp] = s.trim().split('\n\n');
  return [lt.split(', '), lp.split('\n')];
}
parse(sample)

// %%
// some own regexp like impl

type Node = Map<string, Node | null>;
const END = '$';

const buildParseTree = (towels: string[]): Node => {
  const startNode: Node = new Map();
  for (const towel of towels) {
    let node = startNode;
    for (const c of towel) {
      node = node.setdefault(c, new Map())!
    }
    node.set(END, null);
  }
  return startNode;
}
buildParseTree(parse(sample)[0])

// %%
const isPossibleForTowels = (towels: string[]) => {
  const matchTree = buildParseTree(towels);
  const isPossible = (pat: string, node: Node, idx: number): boolean => {
    let currNode = node;
    let i = idx;
    // log(''.padStart(idx), '-- isPossible', pat, idx, node);
    while(i < pat.length) {
      // log(''.padStart(idx), '---- loop', i, 'check for', pat[i])
      const hasEnd = currNode.has(END);
      if (currNode.has(pat[i])) {
        if (hasEnd) {
          // log(''.padStart(idx), '---- has', pat[i], 'and END')
          return isPossible(pat, currNode.get(pat[i])!, i + 1)
              || isPossible(pat, matchTree, i + 1);
        } else {
          // log(''.padStart(idx), '---- has', pat[i])
          currNode = currNode.get(pat[i])!;
          ++i;
        }
      } else if (hasEnd) {
        // log(''.padStart(idx), '---- has END')
        currNode = matchTree;
      } else {
        // log(''.padStart(idx), '---- no match')
        return false;
      }
    }
    // log (''.padStart(idx), '-- isPossible end with node that has END?', currNode.has(END))
    return currNode.has(END);
  }
  return (pat: string) => isPossible(pat, matchTree, 0);
}
{
  const [tl, pt] = parse(sample);
  const expected = [true, true, true, true, false, true, true, false];
  const isPossible = isPossibleForTowels(tl);
  lognb(tl.join(','));
  // isPossible("brwrr")
  for (let i = 0; i < pt.length; ++i) {
    lognb(pt[i], "->", isPossible(pt[i]), (isPossible(pt[i]) === expected[i] ? "===" : "=/="), expected[i])
  }
}


// %%

const solveA = (s: string) => {
  const [towels, patterns] = parse(s);
  const isPossible = isPossibleForTowels(towels);
  let allPossible = 0;
  for (let i = 0; i < patterns.length; ++i) {
    log(`Testing ${i+1}/${patterns.length} -> ${patterns[i]} ...`)
    performance.mark('TEST');
    const possible = isPossible(patterns[i]);
    log(`${ansi.cpl()}Testing ${i+1}/${patterns.length} -> ${patterns[i]} -> ${possible} in ${performance.measure("t", 'TEST').duration}`);
    possible && (++allPossible);
  }
  return allPossible;
}
assertEquals(solveA(sample), 6);

// %%
// too slow... and got stroke on one example -_-
// console.log('Sol A:', solveA(data));

// %%
// this one is much better turns out ^^
const isPossibleForTowels2 = (towels: string[]) => {
  const matchTree = buildParseTree(towels);

  const isPossible = (pat: string): boolean => {
    let nodes: Set<Node> = new Set([matchTree]);
    for (let i = 0; i < pat.length; ++i) {
      const char = pat[i];
      const nNodes = new Set<Node>();;
      for (const node of nodes) {
        if (node.has(char)) nNodes.add(node.get(char)!);
        if (node.has(END) && matchTree.has(char)) nNodes.add(matchTree.get(char)!);
      }
      if (nNodes.size === 0) return false;
      nodes = nNodes;
    }
    for (const n of nodes) {
      if (n.has(END)) return true;
    }
    return false;
  }
  return isPossible;
}
{
  const [tl, pt] = parse(sample);
  const expected = [true, true, true, true, false, true, true, false];
  const isPossible = isPossibleForTowels2(tl);
  lognb(tl.join(','));
  // isPossible("brwrr")
  for (let i = 0; i < pt.length; ++i) {
    lognb(pt[i], "->", isPossible(pt[i]), (isPossible(pt[i]) === expected[i] ? "===" : "=/="), expected[i])
  }
}

const solveA2 = (s: string) => {
  const [towels, patterns] = parse(s);
  const isPossible = isPossibleForTowels2(towels);
  let allPossible = 0;
  log('Method 2');
  for (let i = 0; i < patterns.length; ++i) {
    log(`Testing ${i+1}/${patterns.length} -> ${patterns[i]} ...`)
    performance.mark('TEST');
    const possible = isPossible(patterns[i]);
    log(`${ansi.cpl()}Testing ${i+1}/${patterns.length} -> ${patterns[i]} -> ${possible} in ${performance.measure("t", 'TEST').duration}`);
    possible && (++allPossible);
  }
  return allPossible;
}
assertEquals(solveA2(sample), 6);
console.log('Sol A:', solveA2(data));

// %%

{
  // Built in regexp perf against own...
  const [tlD] = parse(sample);
  const reD = new RegExp('^('+tlD.join('|')+')+$')
  const isPossibleD = (t: string) => !!t.match(reD)
  const isPossibleCustom = isPossibleForTowels(tlD);
  const isPossibleCustom2 = isPossibleForTowels2(tlD);
  // these are strings that for some reason both my and regexp impl struggle with when looping for solutions...
  // but here, when run in isolation - no issues at all ^^
  // Turns out, built in regex is as bad at this as my first attemt xD
  const testOnStr = [
    // 'urbwguuwguruururrubbrbuwrrurrruuwubgruggb',
    // 'ugwbuuuwgrwggwwuwguubrgubuugugbbrbruwuwrubwrbugggb',
    // 'wrrbggwwwgwwwugrgwuurbrurwrwbgubwrbgruwbwgrwrrwuuwrwggb',
    // 'wbgugugugurrggrwggubwwuwbbggwbgrrwruwwuuuubwbbrbrbgggbgggb',
    // 'rbrubuuggwbwgbwbgugwrrrwgbgugrwuubwgwwgurgbwrbuuurbwguuwggb',
    // 'bwbbrrgrrbrggubuggwgguguburbbgbgrruggugbggggb',
    'wbwruugggwuugwbgwburgggbwbbrrgbwburbgrgbwggguubgwrggbwgb',  // there is issue with this one in solve1... but only after it run for 2h ^^
  ];
  for (let testStr of testOnStr) {
    lognb("check perf", testStr)
    performance.mark(testStr+'re');
    const outre = isPossibleD(testStr)
    lognb(`-- RegExp took ${performance.measure("t", testStr+'re').duration} ms with ${outre}`);
    performance.mark(testStr+'custom');
    const outcustom = isPossibleCustom(testStr)
    lognb(`-- custom took ${performance.measure("t", testStr+'custom').duration} ms with ${outcustom}`);
    performance.mark(testStr+'custom2');
    const outcustom2 = isPossibleCustom2(testStr)
    lognb(`-- custom2 took ${performance.measure("t", testStr+'custom2').duration} ms with ${outcustom2}`);
  }
}
// %%

// slight change for part 2
// inspired by https://swtch.com/~rsc/regexp/regexp1.html
// ... only after reimplementing it this way, i've notticed whts the issue and simple memo would solve my recursive impl xD
const getPossibilitesForTowels = (towels: string[]) => {
  const matchTree = buildParseTree(towels);

  const getPossibilites = (pat: string): number => {
    let nodes: Map<Node, number> = new Map([[matchTree, 1]]);
    for (let i = 0; i < pat.length; ++i) {
      const char = pat[i];
      const nNodes = new Map();
      for (const [node, count] of nodes) {
        if (node.has(char)) {
          const nn = node.get(char)!;
          nNodes.set(nn, count + (nNodes.get(nn) ?? 0));
        }
        if (node.has(END) && matchTree.has(char)) {
          const nn = matchTree.get(char)!;
          nNodes.set(nn, count + (nNodes.get(nn) ?? 0));
        }
      }
      if (nNodes.size === 0) return 0;
      nodes = nNodes;
    }
    let out = 0;
    for (const [n, c] of nodes) {
      if (n.has(END)) out += c;
    }
    return out;
  }
  return getPossibilites;
}
{
  const [tl, pt] = parse(sample);
  const expected = [2, 1, 4, 6, 0, 1, 2, 0];
  const isPossible = isPossibleForTowels2(tl);
  const getPosibilities = getPossibilitesForTowels(tl);
  lognb(tl.join(','));
  // isPossible("brwrr")
  for (let i = 0; i < pt.length; ++i) {
    lognb(pt[i], "->", isPossible(pt[i]) ? 'P' : 'N', getPosibilities(pt[i]), (getPosibilities(pt[i]) === expected[i] ? "===" : "=/="), expected[i])
  }
}

// %%

const solveB = (s: string) => {
  const [towels, patterns] = parse(s);
  const getPosibilities = getPossibilitesForTowels(towels);
  let allPossibilities = 0;
  log('Method 2');
  for (let i = 0; i < patterns.length; ++i) {
    log(`Testing ${i+1}/${patterns.length} -> ${patterns[i]} ...`)
    performance.mark('TEST');
    const pos = getPosibilities(patterns[i]);
    log(`${ansi.cpl()}Testing ${i+1}/${patterns.length} -> ${patterns[i]} -> ${pos} in ${performance.measure("t", 'TEST').duration}`);
    allPossibilities += pos;
  }
  return allPossibilities;
}
assertEquals(solveB(sample), 16);

// %%

console.log("Sol B:", solveB(data));
