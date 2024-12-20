
import { BinaryHeap, ascend } from "@std/data-structures";
import { assertEquals } from "@std/assert/equals";

export type AStarSearchMode = 'singleBest' | 'allBest' | 'all';
export type AStarSolution<T> = [dist: number, path: T[]];
export type AStarEvent = 'queued' | 'checking' | 'skipped' | 'foundEnd';

export type AStarParams<T> = {
  /** Start node */
  start: T;
  /** End node, or func checking for end node */
  isEnd: (n: T, nHash: string) => boolean;
  /** Get neighbours of node, with distance to them from given node, this or nbs is required */
  nbsDists?: (n: T) => [node: T, dist: number][];
  /** Get neighbours of node, this or nbsDists is required */
  nbs?: (n: T) => T[];
  /** Dist function, by default returns 1 */
  dist?: (a: T, b: T) => number;
  /** Score function - e.g. distance to target, by default it returns 0 (so, thus works similar to Dijkstra) */
  score?: (n: T) => number;
  /** Optional hash function, to use node as key (JSON.stringify used if not provided) */
  hash?: (n: T) => string;
  /** Search mode, one of: singleBest (default) - get single best solution; allBest - find all top scored solutions; all - get all solutions */
  searchMode?: AStarSearchMode;
  /** Event listener to extract information durring processing */
  listener?: (evt: AStarEvent, node: AStarPathNode<T>) => void;
}

export type AStarPathNode<T> = {
  state: T,
  hash: string,
  dist: number,
  factor: number, // dist + score
  parent: AStarPathNode<T> | null,
}

export function aStar<T>({start, isEnd, nbsDists, nbs, dist, hash, score, searchMode = 'singleBest', listener}: AStarParams<T>): AStarSolution<T>[] {
  hash ??= (n: T) => n !== null && typeof n === "object" ? JSON.stringify(n, Object.keys(n).sort()) : JSON.stringify(n);
  if (!nbsDists && !nbs) throw new Error("nbsDists or nbs required");

  const startHash = hash(start);
  if (isEnd(start, startHash)) return [[0, [start]]];
  const emitEvent = listener ?? (() => {});

  nbsDists ??= (a: T) => nbs!(a).map(n => dist !== undefined ? [n, dist(a, n)] : [n, 1])
  score ??= () => 0;

  const queue = new BinaryHeap<AStarPathNode<T>>(({factor: a}, {factor: b}) => ascend(a, b));
  queue.push({state: start, dist: 0, factor: score(start), hash: startHash, parent: null});
  emitEvent("queued", queue.peek()!);

  const seen = new Map<string, number>();
  seen.set(startHash, 0);

  const solutions: AStarSolution<T>[] = [];

  while (queue.length > 0) {
    const node = queue.pop()!;
    emitEvent("checking", node);
    if (isEnd(node.state, node.hash)) {
      emitEvent("foundEnd", node);
      if (searchMode === 'singleBest') {
        return [[node.dist, reconstructPath(node)]];
      }
      if (searchMode === 'all' || solutions.length === 0 || solutions[0][0] === node.dist) {
        solutions.push([node.dist, reconstructPath(node)]);
      }
      continue;
    }
    // if (node.dist > (seen.get(node.hash) ?? Infinity) || (searchMode === 'allBest' && solutions.length > 0 && node.dist > solutions[0][0])) {
    if (node.dist > (seen.get(node.hash) ?? Infinity)) {
      emitEvent("skipped", node);
      continue;
    }

    for (const [nb, d] of nbsDists(node.state)) {
      const nbDist = node.dist + d;
      const nbHash = hash(nb);
      const sn = seen.get(nbHash) ?? Infinity;
      if (sn > nbDist || (sn === nbDist && searchMode !== "singleBest")) {
        queue.push({
          state: nb,
          dist: nbDist,
          factor: nbDist + score(nb),
          hash: nbHash,
          parent: node
        });
        seen.set(nbHash, nbDist);
      } else {
        emitEvent("skipped", {state: nb, dist: nbDist, factor: -1, hash: nbHash, parent: node});
      }
    }
  }
  return solutions;
}

function reconstructPath<T>(end: AStarPathNode<T>): T[] {
  const path: T[] = [];
  let node: AStarPathNode<T> | null = end;
  while (node !== null) {
    path.unshift(node.state);
    node = node.parent;
  }
  return path;
}
