const isBrowser = typeof window !== "undefined" && typeof window.document !== "undefined";

const day14sample = `
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
`;


export function day14s() {
    day14(day14sample);
}

export function day14(data: string) {
    console.log("go with html: \n> npm run dev\n open http://localhost:5173/");
    // const rocksSpec = dataToPointsSpec(data);
    // const rocks = pointsSpecToPoints(rocksSpec);
    // const cave = new Cave(new Point(500, 0), rocks);

    // while (!cave.caveFull) {
    //     cave.step();
    // }

    // console.log(cave.toString());
    // console.log('day 14:', cave.restingSandCount);
}

class Point {
    constructor(public x: number = 0, public y: number = 0) {
    }
    toString() {
        return '' + this.x + ',' + this.y;
    }
    static fromString(s: string): Point {
        const [x, y] = s.split(',');
        return new Point(parseInt(x), parseInt(y));
    }
}

enum Item {
    ROCK,
    SAND,
    SOURCE
}

class CaveTile extends Point {
    constructor(public type: Item, x: number = 0, y: number = 0) {
        super(x, y);
        this.type = type;
    }

    toString() {
        switch (this.type) {
            case Item.ROCK:
                return '#';
            case Item.SOURCE:
                return '+';
            case Item.SAND:
                return 'o';
            default:
                return '.';
        }
    }
}

class Cave {

    items: Map<string, CaveTile>;
    sandSource: CaveTile;
    top: number = 0;
    bottom: number = 0;
    left: number = 0;
    right: number = 0;

    fallingSand: CaveTile;
    restingSandCount: number = 0;
    caveFull: boolean = false;

    constructor(sandSource: Point, rocks: Point[]) {
        this.items = new Map(rocks.map(p => [p.toString(), new CaveTile(Item.ROCK, p.x, p.y)]));
        this.sandSource = new CaveTile(Item.SOURCE, sandSource.x, sandSource.y);
        this.items.set(sandSource.toString(), this.sandSource);

        this.left = this.right = sandSource.x;
        this.top = this.bottom = sandSource.y;
        for (let i = 0; i < rocks.length; i++) {
            if (rocks[i].y < this.top) {
                this.top = rocks[i].y;
            }
            if (rocks[i].y > this.bottom) {
                this.bottom = rocks[i].y;
            }
            if (rocks[i].x < this.left) {
                this.left = rocks[i].x;
            }
            if (rocks[i].x > this.right) {
                this.right = rocks[i].x;
            }
        }
    }

    step() {
        if (this.caveFull) {
            console.log("Cave full!");
            return;
        }
        
        if (!this.fallingSand) {
            this.fallingSand = new CaveTile(Item.SAND, this.sandSource.x, this.sandSource.y);
            // console.log("New falling sand", this.fallingSand);
        }

        if (this.fallingSand && this.fallingSand.y > this.bottom) {
            isBrowser && console.log("Sand falling over! Cave full with", this.restingSandCount, 'grains of sand at rest');
            this.fallingSand = null;
            this.setCaveFull();
            return;
        }
        
        // console.log('test sand at', this.fallingSand.x, this.fallingSand.y)
        // console.log('below is', this.isFree(this.fallingSand.x, this.fallingSand.y + 1))
        // console.log('left is', this.isFree(this.fallingSand.x - 1, this.fallingSand.y + 1))
        // console.log('right is', this.isFree(this.fallingSand.x + 1, this.fallingSand.y + 1))
        if (this.isFree(this.fallingSand.x, this.fallingSand.y + 1)) {
            this.fallingSand.y += 1;
            // console.log("Sand falling stright down", this.fallingSand);
        }
        else if (this.isFree(this.fallingSand.x - 1, this.fallingSand.y + 1)) {
            this.fallingSand.x -= 1;
            this.fallingSand.y += 1;
            // console.log("Sand falling left", this.fallingSand);
        }
        else if (this.isFree(this.fallingSand.x + 1, this.fallingSand.y + 1)) {
            this.fallingSand.x += 1;
            this.fallingSand.y += 1;
            // console.log("Sand falling right", this.fallingSand);
        }
        else {
            this.items.set(this.fallingSand.x + ',' + this.fallingSand.y, this.fallingSand);
            if (this.fallingSand.x === this.sandSource.x && this.fallingSand.y === this.sandSource.y) {
                this.setCaveFull();
            }
            this.fallingSand = null;
            this.restingSandCount++;
            // console.log("Sand settling", this.restingSandCount);
        }
    }

    protected setCaveFull(): void {
        this.caveFull = true;
    }

    get(x: number, y: number): CaveTile {
        return this.items.get(x + ',' + y);
    }

    isFree(x: number, y: number): boolean {
        return !this.items.has(x + ',' + y);
    }

    getAsChar(x: number, y: number): string {
        return this.get(x, y)?.toString() || '.';
    }

    toString() {
        const parts = [];
        for (let y = this.top; y <= this.bottom; ++y) {
            const row = [];
            for (let x = this.left; x <= this.right; ++x) {
                row.push(this.getAsChar(x, y));
            }
            parts.push(row.join(''));
        }
        return parts.join('\n');
    }
}

class FloredCave extends Cave {
    constructor(sandSource: Point, rocks: Point[]) {
        super(sandSource, rocks);

        this.left -= 2;
        this.right += 2;
        this.bottom += 2;

        for (let x = this.left; x <= this.right; ++x) {
            const extraWall = new CaveTile(Item.ROCK, x, this.bottom);
            this.items.set(new Point(x, this.bottom).toString(), extraWall);
            console.log("added extra wall", extraWall.toString())
        }
        for (let y = this.top; y < this.bottom; ++y) {
            const extraWallL = new CaveTile(Item.ROCK, this.left, y);
            this.items.set(new Point(this.left, y).toString(), extraWallL);
            const extraWallR = new CaveTile(Item.ROCK, this.right, y);
            this.items.set(new Point(this.right, y).toString(), extraWallR);
        }
    }

    protected setCaveFull(): void {
        super.setCaveFull();

        const leftStack = this.countExtraStack(this.left);
        const rightStack = this.countExtraStack(this.right);
        console.log(`extra stacks - left: ${leftStack} right: ${rightStack}`);

        this.restingSandCount += leftStack + rightStack;
    }

    private countExtraStack(x: number): number {
        const xDiff = Math.abs(this.sandSource.x - x);
        const height = this.bottom - xDiff;
        return (height + 1) * height / 2;
    }
}

function dataToPointsSpec(data: string): Point[][] {
    return data.trim().split('\n').map(line => line.split(' -> ').map(p => Point.fromString(p)));
}

function pointsSpecToPoints(spec: Point[][]): Point[] {
    const points = [];

    for (const row of spec) {
        for (let i = 1; i < row.length; ++i) {
            points.push(...pointsOnLine(row[i - 1], row[i]));
        }
    }

    return points;
}

function pointsOnLine(start: Point, end: Point): Point[] {
    if (start.x === end.x) {
        return range(start.y, end.y).map(y => new Point(start.x, y));
    }
    if (start.y === end.y) {
        return range(start.x, end.x).map(x => new Point(x, start.y));
    }
    throw new Error("line spec is not stright " + start.toString() + " -> " + end.toString());
}

function range(a: number, b: number): number[] {
    const aa = Math.min(a, b);
    const bb = Math.max(a, b);
    const arr = [];
    for (let i = aa; i <= bb; ++i) {
        arr.push(i);
    }
    return arr;
}


// HTML RENDER

const CANVAS_SCALE = 5;
const COLOR_MAP = {
    [Item.ROCK]: "#118ab2",
    [Item.SOURCE]: "#06d6a0",
    [Item.SAND]: "#ffd166"
}
const FALLING_SAND_COLOR = "#ef476f";

if (isBrowser) {
    const isVariantB: boolean = document.location.hash.indexOf('b') > -1;
    const withSample: boolean = document.location.hash.indexOf('s') > -1;
    document.getElementById("variant").innerHTML = isVariantB ? 'b' : 'a';
    animateInBrowser(withSample, isVariantB);
}

async function animateInBrowser(withSample: boolean, variantB: boolean) {
    const data = withSample ? day14sample : (await (await fetch('../data/day14.txt')).text());
    const rocksSpec = dataToPointsSpec(data);
    const rocks = pointsSpecToPoints(rocksSpec);
    
    const CaveType = variantB ? FloredCave : Cave;

    const cave = new CaveType(new Point(500, 0), rocks);

    const padding = 5;
    const width = cave.right - cave.left + 1 + 2 * padding;
    const height = cave.bottom - cave.top + 1 + 2 * padding;

    const sandCounter = document.getElementById("sandCount");

    const canvas = document.getElementById('cave') as HTMLCanvasElement;
    canvas.width = width;
    canvas.height = height;
    canvas.style.width = width * CANVAS_SCALE + 'px';
    canvas.style.height = height * CANVAS_SCALE + 'px';
    const ctx = canvas.getContext('2d');

    ctx.clearRect(0, 0, width, height);

    drawCave(ctx, cave, padding, width, height);

    const render = () => {
        if (cave.caveFull) {
            return;
        }
        cave.step();
        
        if (cave.restingSandCount > 30) {
            while (cave.fallingSand) {
                cave.step();
            }
        }
        drawCave(ctx, cave, padding, width, height);
        sandCounter.innerHTML = '' + cave.restingSandCount;
        requestAnimationFrame(render);
    }
    requestAnimationFrame(render);
}

function drawCave(ctx: CanvasRenderingContext2D, cave: Cave, padding: number, width: number, height: number) {
    ctx.fillStyle = '#073b4c';
    ctx.fillRect(0, 0, width, height);
    
    const offsetX = padding - cave.left;
    const offestY = padding - cave.top;

    for (let [point, item] of cave.items) {
        ctx.fillStyle = COLOR_MAP[item.type];
        ctx.fillRect(item.x + offsetX, item.y + offestY, 1, 1);
    }

    if (cave.fallingSand) {
        ctx.fillStyle = FALLING_SAND_COLOR;
        ctx.fillRect(cave.fallingSand.x + offsetX, cave.fallingSand.y + offestY, 1, 1);
    }
}