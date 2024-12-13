
const day09sample = `
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
`;

export function day09s() {
    day09(day09sample);
}

export function day09(data: string) {
    const parsed = data.trim().split('\n');

    const rope2 = new Rope(2);
    const rope10 = new Rope(10);

    for (const step of parsed) {
        rope2.move(step);
        rope10.move(step);
        // console.log('after step: ' + step);
        // console.log('head: ', rope.hx, rope.hy, 'tail: ', rope.tx, rope.ty);
    }

    console.log('day09 a:', (new Set(rope2.tailTraice.map(t => t[0] * 1000000 + t[1]))).size);
    console.log('day09 b:', (new Set(rope10.tailTraice.map(t => t[0] * 1000000 + t[1]))).size);
}
class Rope {

    knots: number[][];

    hx = 0;
    hy = 0;
    tx = 0;
    ty = 0;

    tailTraice = [[0, 0]];

    constructor(length: number) {
        this.knots = Array.from({ length }, () => [0, 0]);
    }

    move(spec: string) {
        const [dir, nstr] = spec.split(' ');
        const n = parseInt(nstr, 10);

        let func = null;

        switch (dir) {
            case 'U': func = () => this.moveHead(0, 1); break;
            case 'D': func = () => this.moveHead(0, -1); break;
            case 'R': func = () => this.moveHead(1, 0); break;
            case 'L': func = () => this.moveHead(-1, 0); break;
        }

        for (let i = 0; i < n; i++) {
            func();
        }
    }

    private moveHead(x: number, y: number) {
        this.knots[0][0] += x;
        this.knots[0][1] += y;

        for (let i = 1; i < this.knots.length; i++) {
            this.moveKnot(i);
        }
        if (this.knots.last()[0] !== this.tailTraice.last()[0] || this.knots.last()[1] !== this.tailTraice.last()[1]) {
            this.tailTraice.push([...this.knots.last()]);
        }
    }

    private moveKnot(idx: number) {
        const [px, py] = this.knots[idx - 1];
        const [kx, ky] = this.knots[idx];

        const dx = px - kx;
        const dxa = Math.abs(dx);
        const dy = py - ky;
        const dya = Math.abs(dy);

        if (dxa > 1 || dya > 1) {
            this.knots[idx][0] += dxa > 0 ? (dx / dxa) : 0;
            this.knots[idx][1] += dya > 0 ? (dy / dya) : 0;
        }
    }
}
