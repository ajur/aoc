
const day05sample = `
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
`;

export function day05s() {
    day05(day05sample);
}

export function day05(data: string) {
    const [layoutRaw, movesRaw] = data.split('\n\n');
    const supply = CrateStacks.from(layoutRaw);
    const moves = movesRaw.trim().split('\n').map(Move.from);

    console.log('day05 a:', supply.copy().moveAllByOne(moves).topCrates());
    console.log('day05 b:', supply.copy().moveAll(moves).topCrates());
}
class Move {
    constructor(public quantity: number, public from: number, public to: number) {
    }
    static from(spec: string) {
        const [_, quantity, from, to] = /move (\d+) from (\d+) to (\d+)/.exec(spec);
        return new Move(parseInt(quantity), parseInt(from), parseInt(to));
    }
}
class CrateStacks {
    constructor(public stacks: string[][]) {
    }

    static from(raw: string) {
        const rawLines = raw.split('\n');
        const stacksCount = parseInt(rawLines.last().trim().split(' ').last());
        const maxStack = rawLines.length - 1;

        const cratesLines = rawLines.slice(0, -1).map(l => l.inGroupOf(4).map(g => g.trim()).map(g => g.length === 3 ? g[1] : null));
        cratesLines.reverse();
        const stacks = [];
        for (let i = 0; i < stacksCount; i++) {
            const stack = [];
            for (let j = 0; j < maxStack; j++) {
                if (cratesLines[j][i]) {
                    stack.push(cratesLines[j][i]);
                }
            }
            stacks.push(stack);
        }
        return new CrateStacks(stacks);
    }

    copy(): CrateStacks {
        return new CrateStacks([...this.stacks.map(s => [...s])]);
    }

    toString(): string {
        return this.stacks.map(s => s.join(' ')).join('\n');
    }

    moveAllByOne(spec: Move[]): CrateStacks {
        spec.forEach(m => this.moveByOne(m));
        return this;
    }

    moveByOne(spec: Move): CrateStacks {
        for (let i = 0; i < spec.quantity; ++i) {
            this.stacks[spec.to - 1].push(this.stacks[spec.from - 1].pop());
        }
        return this;
    }

    moveAll(spec: Move[]): CrateStacks {
        spec.forEach(m => this.move(m));
        return this;
    }

    move(spec: Move): CrateStacks {
        this.stacks[spec.to - 1].push(...this.stacks[spec.from - 1].splice(-spec.quantity));
        return this;
    }

    topCrates(): string {
        return this.stacks.map(s => s.last()).join('');
    }
}
