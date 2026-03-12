import { lognb } from '#lib';
// %%

const sample =`
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
`;

type ParsedData = Map<string, string[]>;
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => s.trim()
  .split('\n')
  .map(l => l.trim().split(':').callOnMe(([l, r]) => [l.trim(), r.trim().split(' ')]))
  .callOnMe((d) => new Map(d as [string, string[]][]))

lognb(parse(sample))

// %%

const allPaths = (map: ParsedData, from: string, to: string, path: string[]): string[][] => {
  if (from === to) return [[...path, to]];
  const outs = map.get(from) ?? [];
  return outs.filter(o => !path.includes(o)).flatMap(o => allPaths(map, o, to, [...path, from]));
};

allPaths(parse(sample), "you", "out", [])

// %%

const solveA = (pd: ParsedData) => {
  const sol = allPaths(pd, "you", "out", []);
  sol.length < 20 && lognb(sol.map(p => p.join('->')));
  return sol.length;
}

lognb(solveA(parse(sample)));

// %%

const data = await Deno.readTextFile("./data/day11.txt");

console.log("Sol A:", solveA(parse(data)))

// %%

const sampleB = `
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
`

const countPaths = (map: ParsedData, from: string, to: string, visited: Set<string>, cutAt: Set<string>): number => {
  if (from === to) return 1;
  if (cutAt.has(from)) return 0;
  const outs = (map.get(from) ?? []).filter(o => !visited.has(o));
  if (outs.length === 0) return 0;
  const newVisited = new Set([...visited, from]);
  return outs.map(o => countPaths(map, o, to, newVisited, cutAt)).sum();
};

lognb(countPaths(parse(sampleB), "svr", "out", new Set(), new Set()))
lognb(countPaths(parse(sampleB), "svr", "out", new Set(), new Set(["fft"])))

// %%

const stepsSample = [["fft"], ["dac"]];

const solveB = (pd: ParsedData, steps: string[][]) => {
  steps.push(["out"]);
  const pathsTo = new Map([["svr", 1]]);
  let checkFrom = ["svr"];

  for (let i = 0; i < steps.length; ++i) {
    const nextStepNodes = steps[i];
    const hardStopAt = new Set([...nextStepNodes, ...(steps[i + 1] ?? [])]);

    for (const to of nextStepNodes) {
      let allPaths = 0;
      for (const from of checkFrom) {
        const paths = countPaths(pd, from, to, new Set(), hardStopAt);
        allPaths += paths * pathsTo.get(from)!;
      }
      pathsTo.set(to, allPaths);
    }
    checkFrom = nextStepNodes;
  }
  return pathsTo.get('out');
}

lognb(solveB(parse(sampleB), stepsSample))
// %%

// extracted from data graph, and hardcoded here - thats only way untill i learn how to do this properly :/
const stepsData = [
  ["qxn", "jjm", "voy"],
  ["fft"],
  ["vyd", "caz", "fta", "zrf", "wgs"],
  ["jxx", "qdw", "bhg", "gep"],
  ["qma", "qxk", "rnz", "zet", "fhn"],
  ["dac"],
  ["you", "txy", "itr"]
];

console.log("Sol B:", solveB(parse(data), stepsData))
