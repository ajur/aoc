
import { BinaryHeap, ascend } from "@std/data-structures";
import { assertEquals } from "@std/assert/equals";

export type AStarParams<T> = {
  /** Start node */
  start: T;
  /** End node */
  end: T;
  /** Get neighbours of node, with distance to them from given node, this or nbs is required */
  nbsDists?: (n: T) => [node: T, dist: number][];
  /** Get neighbours of node, this or nbsDists is required */
  nbs?: (n: T) => T[];
  /** Dist function, by default returns 1 */
  dist?: (a: T, b: T) => number;
  /** Score function - e.g. distance to target, by default it returns 0 (so, thus works similar to Dijkstra) */
  score?: (n: T, target: T) => number;
  /** Optional hash function, to use node as key (JSON.stringify used if not provided) */
  hash?: (n: T) => string;
  /** Event listener to extract information durring processing */
  listener?: (s: string) => void;
}

type PathNode<T> = {
  state: T,
  hash: string,
  dist: number,
  factor: number, // dist + score
  parent: PathNode<T> | null,
}

export function aStar<T>({start, end, nbsDists, nbs, dist, hash, score}: AStarParams<T>): [number, T[]] {
  hash ??= (n: T) => n !== null && typeof n === "object" ? JSON.stringify(n, Object.keys(n).sort()) : JSON.stringify(n);
  if (!nbsDists && !nbs) throw new Error("nbsDists or nbs required");

  const startHash = hash(start);
  const endHash = hash(end);
  if (startHash === endHash) return [0, [start]];

  nbsDists ??= (a: T) => nbs!(a).map(n => dist !== undefined ? [n, dist(a, n)] : [n, 1])
  score ??= () => 0;

  const queue = new BinaryHeap<PathNode<T>>(({factor: a}, {factor: b}) => ascend(a, b));
  queue.push({state: start, dist: 0, factor: score(start, end), hash: startHash, parent: null});

  const seen = new Map<string, number>();
  seen.set(startHash, 0);

  while (queue.length > 0) {
    const node = queue.pop()!;
    if (node.hash === endHash) {
      return [node.dist, reconstructPath(node)];
    }
    if (node.dist > (seen.get(node.hash) ?? Infinity)) {
      continue;
    }

    for (const [nb, d] of nbsDists(node.state)) {
      const nbDist = node.dist + d;
      const nbHash = hash(nb);
      const sn = seen.get(nbHash) ?? Infinity;
      if (sn > nbDist) {
        queue.push({
          state: nb,
          dist: nbDist,
          factor: nbDist + score(nb, end),
          hash: nbHash,
          parent: node
        });
        seen.set(nbHash, nbDist);
      }
    }
  }
  return [-1, []];
}

function reconstructPath<T>(end: PathNode<T>): T[] {
  const path: T[] = [];
  let node: PathNode<T> | null = end;
  while (node !== null) {
    path.unshift(node.state);
    node = node.parent;
  }
  return path;
}


Deno.test("A Star nbsDists", () => {
  const graph: Record<string, [string, number][]> = {
    A: [["B", 1], ["C", 4]],
    B: [["C", 2], ["D", 5]],
    C: [["D", 1]],
    D: [],
  };

  const result = aStar({
    start: 'A',
    end: 'D',
    nbsDists: (n: string) => graph[n]
  });
  assertEquals(result, [4, ['A', 'B', 'C', 'D']]);
});

Deno.test("A Star nbs + dist & hash", () => {
  const graph: Record<string, Record<string, number>> = {
    A: {B: 1, C: 4},
    B: {C: 2, D: 5},
    C: {D: 1},
    D: {},
  };

  const result = aStar({
    start: 'A',
    end: 'D',
    nbs: (n: string) => Object.keys(graph[n]),
    dist: (a: string, b: string) => (graph[a][b] || graph[b][a]),
    hash: (n: string) => n
  });
  assertEquals(result, [4, ['A', 'B', 'C', 'D']]);
});
