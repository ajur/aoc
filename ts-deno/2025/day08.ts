import { asInt, lognb } from '#lib';
// %%

const sample =`
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
`;

type Vec3 = [x: number, y: number, z: number];
type ParsedData = Vec3[];
type Parser = (s: string) => ParsedData;

const parse: Parser = (s: string) => s.trim()
  .split('\n')
  .map(l => l.split(',').map(asInt) as Vec3);

lognb(parse(sample))

// %%

type DistSpec = [sqDist: number, v1idx: number, v2idx: number];

const vec3DistSq = (a: Vec3, b: Vec3) => (a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2;

const solveA = (nodes: ParsedData, joinsNum: number) => {
  const dolog = joinsNum < 20;
  const dists: DistSpec[] = [];
  for (let i = 0; i < nodes.length; ++i) {
    for (let j = i + 1; j < nodes.length; ++j) {
      const a = nodes[i];
      const b = nodes[j];
      dists.push([vec3DistSq(a, b), i, j]);
    }
  }
  dists.sort(([a], [b]) => a - b);
  dolog && lognb(dists);
  const joints: (number[] | null)[] = nodes.map(() => []);
  for (let i = 0; i < joinsNum; ++i) {
    const [_, a, b] = dists[i];
    joints[a]!.push(b);
    joints[b]!.push(a);
  }
  dolog && lognb(joints);
  const collect = (ni: number): number[] => {
    // console.log('checking', ni, 'joints', joints[ni]);
    if (joints[ni] === null) return [ni];
    if (joints[ni].length === 0) {
      joints[ni] = null;
      return [ni];
    }
    const jni = joints[ni];
    joints[ni] = null;
    const out = [];
    for (const j of jni) {
      out.push(...jni, ...collect(j));
    }
    return out;
  };
  const circuits: Set<number>[] = [];
  for (let i = 0; i < joints.length; ++i) {
    if (joints[i] !== null) {
      circuits.push(new Set(collect(i)));
    }
  }
  dolog && lognb("circuits", circuits);
  return circuits.map(s => s.size).sort((a,b)=> b-a).slice(0, 3).reduce((m, n) => m * n, 1);
}

solveA(parse(sample), 10);

// %%

const data = await Deno.readTextFile("./data/day08.txt");

console.log("Sol A:", solveA(parse(data), 1000))

// %%

const solveB = (nodesIn: ParsedData) => {
  const dists: DistSpec[] = [];
  for (let i = 0; i < nodesIn.length; ++i) {
    for (let j = i + 1; j < nodesIn.length; ++j) {
      const a = nodesIn[i];
      const b = nodesIn[j];
      dists.push([vec3DistSq(a, b), i, j]);
    }
  }
  dists.sort(([a], [b]) => a - b);
  // console.log('dists\n', dists);
  const circuits: Set<number>[] = [];
  const nodeCircuits: (number | null)[] = nodesIn.map(() => null);

  for (let i = 0; i < dists.length; ++i) {
    const [_, a, b] = dists[i];
    const ca = nodeCircuits[a];
    const cb = nodeCircuits[b];
    if (ca === null && cb === null) {
      circuits.push(new Set([a, b]))
      nodeCircuits[a] = nodeCircuits[b] = circuits.length - 1;
    } else if (ca !== null && cb === null) {
      circuits[ca].add(b);
      nodeCircuits[b] = ca;
    } else if (ca === null && cb !== null) {
      circuits[cb].add(a);
      nodeCircuits[a] = cb;
    } else if (ca !== null && cb !== null && ca !== cb) {
      const nci = Math.min(ca, cb);
      const nc = circuits[ca].union(circuits[cb]);
      delete circuits[ca];
      delete circuits[cb];
      circuits[nci] = nc;
      for (const ni of nc.keys()) {
        nodeCircuits[ni] = nci;
      }
    }
    // console.log("after dist", i, "\ncircuits", circuits)
    if (circuits[0].size === nodesIn.length) {
      // console.log(circuits[0]);
      // console.log(nodesIn[a], nodesIn[b])
      return nodesIn[a][0] * nodesIn[b][0];
    }
  }
}

lognb(solveB(parse(sample)))
// %%

console.log("Sol B:", solveB(parse(data)))
