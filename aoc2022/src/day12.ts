import { readFileSync } from "fs";

const FastPriorityQueue = require("fastpriorityqueue");

const day12sample = `
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
`;

export function day12s() {
    day12(day12sample);
}

export function day12(data: string) {
    const grid = HeightMap.fromString(data.trim());

    // console.log(data);

    const path = findPath(grid);

    // console.log(path);
    
    // const pathMap = Array.from({length: grid.map.length}, () => Array.from({length: grid.map[0].length}, () => '.'));
    // for (const node of path) {
    //     pathMap[node.y][node.x] = 'X'
    // }
    // console.log(pathMap.map(row => row.join('')).join('\n'));

    console.log('day12 a:', path.length);

    const pathA = findPathReversed(grid);

    // console.log(pathA);
    // const pathMap = Array.from({length: grid.map.length}, () => Array.from({length: grid.map[0].length}, () => '.'));
    // for (const node of pathA) {
    //     pathMap[node.y][node.x] = 'X'
    // }
    // console.log(pathMap.map(row => row.join('')).join('\n'));

    console.log('day12 b:', pathA.length);
}


function findPath(hmap: HeightMap): GraphNode[] {
    const graph: GraphNode[][] = Array.from(hmap.map, (row, y) => Array.from(row, (h, x) => new GraphNode(x, y, h)));
    const end = graph[hmap.end.y][hmap.end.x];

    const openNodes = new FastPriorityQueue((a: GraphNode, b: GraphNode) => a.score < b.score);

    const start = graph[hmap.start.y][hmap.start.x];
    openNodes.add(start);

    while (!openNodes.isEmpty()) {
    // for (let i = 0; i < 2; ++i) {
        const currentNode = openNodes.poll();

        // console.log('step ', i);
        // console.log(currentNode);

        if (currentNode === end) {
            let curr = currentNode;
            let path = [];
            while (curr.parent) {
                path.push(curr);
                curr = curr.parent;
            }
            return path.reverse();    
        }

        currentNode.closed = true;

        const nns = neighbors(currentNode, graph);

        for (const neighbor of nns) {
            if (neighbor.closed) { 
                continue;
            }
            const ng = currentNode.g + 1;
            const visited = neighbor.visited;

            if (!visited || ng < neighbor.g) {
                neighbor.visited = true;
                neighbor.parent = currentNode;
                neighbor.g = ng;
                neighbor.h = manhattan(neighbor, end);

                if (visited) {
                    openNodes.removeMany((a: GraphNode) => a === neighbor);
                }
                openNodes.add(neighbor);
            }
        }

        // console.log(openNodes);
    }
    return [];
}

function manhattan(a: GraphNode, b: GraphNode) {
    return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

function neighbors(node: GraphNode, graph: GraphNode[][]): GraphNode[] {
    const {x, y, height} = node;
    const ns = [];

    for (let [xx, yy] of [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]) {
        if (graph[yy] && graph[yy][xx] && (graph[yy][xx].height - height) <= 1) {
            ns.push(graph[yy][xx]);
        }
    }
    return ns;
}

function findPathReversed(hmap: HeightMap): GraphNode[] {
    const graph: GraphNode[][] = Array.from(hmap.map, (row, y) => Array.from(row, (h, x) => new GraphNode(x, y, h)));
    const end = graph[hmap.end.y][hmap.end.x];

    const openNodes = new FastPriorityQueue((a: GraphNode, b: GraphNode) => a.g < b.g);

    openNodes.add(end);

    while (!openNodes.isEmpty()) {
    // for (let i = 0; i < 5; ++i) {
        const currentNode = openNodes.poll();

        // console.log('step ', i);
        // console.log(currentNode);

        if (currentNode.height === 1) {
            let curr = currentNode;
            let path = [];
            while (curr.parent) {
                path.push(curr);
                curr = curr.parent;
            }
            return path;
        }

        currentNode.closed = true;

        const nns = neighborsGoingDown(currentNode, graph);

        for (const neighbor of nns) {
            if (neighbor.closed) {
                continue;
            }
            const ng = currentNode.g + 1;
            const visited = neighbor.visited;

            if (!visited || ng < neighbor.g) {
                neighbor.visited = true;
                neighbor.parent = currentNode;
                neighbor.g = ng;

                if (visited) {
                    openNodes.removeMany((a: GraphNode) => a === neighbor);
                }
                openNodes.add(neighbor);
            }
        }

        // console.log(openNodes);
    }

    return [];
}

function neighborsGoingDown(node: GraphNode, graph: GraphNode[][]): GraphNode[] {
    const {x, y, height} = node;
    const ns = [];

    for (let [xx, yy] of [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]) {
        if (graph[yy] && graph[yy][xx] && (height - graph[yy][xx].height) <= 1) {
            ns.push(graph[yy][xx]);
        }
    }
    return ns;
}


class GraphNode {

    parent: GraphNode;
    visited: boolean = false;
    closed: boolean = false;

    h: number = 0; // estimated time to reach target node
    g: number = 0; // total cost of getting to the node

    constructor(
        public x: number,
        public y: number,
        public height: number
    ) {
    }

    // or f - the lower, the better
    get score(): number {
        return this.g + this.h;
    }
}


type Point = {
    x: number,
    y: number
};

class HeightMap {
    constructor(public map: number[][], public start: Point, public end: Point) {
    }

    public static fromString(s: string): HeightMap {
        const map = s.split('\n').map(line => line.split('').map(char => charToHeight(char)));

        const ln = map[0].length + 1;
        const startIdx = s.indexOf('S');
        const start = {
            x: startIdx % ln,
            y: Math.floor(startIdx / ln)
        };
        const endIdx = s.indexOf('E');
        const end = {
            x: endIdx % ln,
            y: Math.floor(endIdx / ln)
        };

        return new HeightMap(map, start, end);
    }
}

function charToHeight(char: string): number {
    if (char === 'S') return 1;
    if (char === 'E') return 26;
    return char.charCodeAt(0) - 96;
}
