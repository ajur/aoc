import { asInt, not } from  "#lib";
import { assert } from "@std/assert";
// %%

const sample = `
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`

type Rule = [before: number, after: number];
type Update = number[];

const parse = (s: string) => {
  const [rules, update] = s.trim().split('\n\n');
  return [
    rules.split('\n').map(l => l.split('|').map(asInt)),
    update.split('\n').map(l => l.split(',').map(asInt))
  ] as [Rule[], Update[]];
}
parse(sample)
// %%

type RuleNumber = {
  before: Set<number>,
  after: Set<number>
};
type RulesMap = Map<number, RuleNumber>;

const getRulesMap = (rules: Rule[]): RulesMap => {
  const rmap: RulesMap = new Map<number, RuleNumber>;
  for (const [a, b] of rules) {
    rmap.setdefault(a, { before: new Set<number>(), after: new Set<number>() }).before.add(b);
    rmap.setdefault(b, { before: new Set<number>(), after: new Set<number>() }).after.add(a);
  }
  return rmap;
}

parse(sample)[0].callOnMe(getRulesMap)

// %%

const isSortedForRules = (rmap: RulesMap) => (update: Update) => {
  for (let i = 0; i < update.length; ++i) {
    if (!rmap.has(update[i])) {
      console.log('rules map has no number', update[i]);
      continue;
    }
    const {before, after} = rmap.get(update[i])!;
    for (let j = 0; j < i; ++j) {
      if (before.has(update[j])) return false;
    }
    for (let j = i + 1; j < update.length; ++j) {
      if (after.has(update[j])) return false;
    }
  }
  return true;
}

const solveA = (s: string) => {
  const [rs, us] = parse(s);
  const rmap = getRulesMap(rs);
  const isSorted = isSortedForRules(rmap);

  return us
    .filter(isSorted)
    .map(u => u[Math.floor(u.length / 2)])
    .sum();
}

solveA(sample);
// %%

const data = await Deno.readTextFile('./data/day05.txt');
console.log('Sol A:', solveA(data));

// %%


const getSorter = (rmap: RulesMap) => (update: Update) => {
  let i = 0;
  outerWhile:
  while (i < update.length) {
    const {before, after} = rmap.get(update[i])!;
    for (let j = 0; j < i; ++j) {
      if (before.has(update[j])) {
        update.insert(j, ...update.remove(i));
        i = j;
        continue outerWhile;
      };
    }
    for (let j = i + 1; j < update.length; ++j) {
      if (after.has(update[j])) {
        update.insert(i, ...update.remove(j));
        continue outerWhile;
      }
    }
    ++i;
  }
  return update;
}

Deno.test("day 5 sorter", () => {
  const [rs, up] = parse(sample)
  const rmap = getRulesMap(rs);
  const isSorted = isSortedForRules(rmap)
  const sortUpdates = getSorter(rmap)

  up.filter(not(isSorted)).forEach(u => {
    const sorted = sortUpdates([...u]);
    console.log(u, isSorted(u), '->', sorted, isSorted(sorted))
    assert(isSorted(sorted));
  });
});

// %%

const solveB = (s: string) => {
  const [rs, up] = parse(s)
  const rmap = getRulesMap(rs);
  const isSorted = isSortedForRules(rmap)
  const sortUpdates = getSorter(rmap)
  return up
    .filter(not(isSorted))
    .map(u => sortUpdates(u))
    .map(u => u[Math.floor(u.length / 2)])
    .sum();
}
solveB(sample)
// %%

console.log('Sol B:', solveB(data));
