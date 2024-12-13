
const day08sample = `
30373
25512
65332
33549
35390
`;

export function day08s() {
    day08(day08sample);
}

export function day08(data: string) {
    const parsed: TallTree[][] = data.trim().split('\n').map(
        l => l.split('').map(d => (new TallTree(parseInt(d, 10))))
    );

    const trees = markVisibleTrees(parsed);
    const biggestCover = markTreeCoverAndReturnHiegst(trees);

    // printTrees(trees);
    console.log('day08 a:', trees.map(tl => tl.map(t => t.isVisible ? 1 : 0).sum()).sum());
    console.log('day08 b:', biggestCover);
}
class TallTree {
    isVisibleFromTop = false;
    isVisibleFromBottom = false;
    isVisibleFromLeft = false;
    isVisibleFromRight = false;
    coverage = 0;

    constructor(public height: number) {
    }

    get isVisible() {
        return this.isVisibleFromTop || this.isVisibleFromBottom || this.isVisibleFromLeft || this.isVisibleFromRight;
    }

    copy(): TallTree {
        const nt = new TallTree(this.height);
        nt.isVisibleFromTop = this.isVisibleFromTop;
        nt.isVisibleFromBottom = this.isVisibleFromBottom;
        nt.isVisibleFromLeft = this.isVisibleFromLeft;
        nt.isVisibleFromRight = this.isVisibleFromRight;
        return nt;
    }
}
function markVisibleTrees(raw: TallTree[][]): TallTree[][] {
    const trees = raw.map(tl => tl.map(t => (t.copy())));
    for (let i = 0; i < trees.length; i++) {
        trees[i][0].isVisibleFromLeft = true;
        trees[i].last().isVisibleFromRight = true;
    }
    for (let i = 0; i < trees[0].length; i++) {
        trees[0][i].isVisibleFromTop = true;
        trees.last()[i].isVisibleFromBottom = true;
    }

    for (let i = 1; i < trees.length - 1; i++) {
        for (let j = 1; j < trees[i].length - 1; j++) {
            checkIfVisible(trees, i, j);
        }
    }

    return trees;
}
function checkIfVisible(trees: TallTree[][], i: number, j: number) {
    const t = trees[i][j];
    const h = t.height;
    for (let n = i - 1; n >= 0; --n) {
        const tt = trees[n][j];
        if (tt.height >= h) {
            break;
        }
        if (tt.isVisibleFromTop) {
            t.isVisibleFromTop = true;
            break;
        }
    }
    for (let n = i + 1; n < trees.length; ++n) {
        const tt = trees[n][j];
        if (tt.height >= h)
            break;
        if (tt.isVisibleFromBottom) {
            t.isVisibleFromBottom = true;
            break;
        }
    }
    for (let n = j - 1; n >= 0; --n) {
        const tt = trees[i][n];
        if (tt.height >= h)
            break;
        if (tt.isVisibleFromLeft) {
            t.isVisibleFromLeft = true;
            break;
        }
    }
    for (let n = j + 1; n < trees[i].length; ++n) {
        const tt = trees[i][n];
        if (tt.height >= h) {
            break;
        }
        if (tt.isVisibleFromRight) {
            t.isVisibleFromRight = true;
            break;
        }
    }
}
function markTreeCoverAndReturnHiegst(trees: TallTree[][]) {
    let biggestCover = 0;
    for (let i = 0; i < trees.length; i++) {
        for (let j = 0; j < trees[i].length; j++) {
            let coverage = checkTreeCover(trees, i, j);
            if (coverage > biggestCover) {
                biggestCover = coverage;
            }
        }
    }
    return biggestCover;
}
function checkTreeCover(trees: TallTree[][], i: number, j: number): number {
    const t = trees[i][j];
    const h = t.height;

    let treesTop = 0;
    let treesBottom = 0;
    let treesLeft = 0;
    let treesRight = 0;

    for (let n = i - 1; n >= 0; --n) {
        ++treesTop;
        if (trees[n][j].height >= h)
            break;
    }
    for (let n = i + 1; n < trees.length; ++n) {
        ++treesBottom;
        if (trees[n][j].height >= h)
            break;
    }
    for (let n = j - 1; n >= 0; --n) {
        ++treesLeft;
        if (trees[i][n].height >= h)
            break;
    }
    for (let n = j + 1; n < trees[i].length; ++n) {
        ++treesRight;
        if (trees[i][n].height >= h)
            break;
    }
    t.coverage = treesTop * treesBottom * treesLeft * treesRight;
    return t.coverage;
}
function printTrees(trees: TallTree[][]) {
    console.log('-- trees:');
    console.log(
        trees.map(
            tl => tl.map(
                t => (t.isVisible ? '\x1b[1m' : '\x1b[2m') + t.height + '\x1b[0m'
            ).join('')
        ).join('\n')
    );
}
