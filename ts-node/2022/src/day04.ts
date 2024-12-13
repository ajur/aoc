
const day04sample = `
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
29-97,28-98
`;

export function day04s() {
    day04(day04sample);
}

export function day04(data: string) {
    const parsed = data.trim()
        .split('\n')
        .map(l => l.split(',').map(Range.from));

    console.log('day04 a:', parsed.map(([r1, r2]) => r1.contains(r2) || r2.contains(r1)).filter(v => v).length);

    parsed.forEach(([r1, r2]) => console.log(r1, r2, r1.overlaps(r2)));
    console.log('day04 b:', parsed.map(([r1, r2]) => r1.overlaps(r2)).filter(v => v).length);
}
class Range {
    constructor(public start: number, public end: number) {
    }
    static from(dashed: string) {
        const [start, end] = dashed.split('-').map(p => parseInt(p, 10));
        return new Range(start, end);
    }

    intersection(other: Range) {
        return new Range(Math.max(this.start, other.start), Math.min(this.end, other.end));
    }

    union(other: Range) {
        return new Range(Math.min(this.start, other.start), Math.max(this.end, other.end));
    }

    contains(other: Range) {
        return this.start <= other.start && this.end >= other.end;
    }

    overlaps(other: Range) {
        return Math.max(this.start, other.start) <= Math.min(this.end, other.end);
    }
}
