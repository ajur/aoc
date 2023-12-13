

import './toolbelt';

const day13sample = `
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
`;

export function day13s() {
    return day13(day13sample);
}

export function day13(data: string) {
    const parsed = data.trim().split('\n\n').map(group => group.split('\n').map(line => JSON.parse(line)));
    
    console.log('day13 a:', parsed.map(([a,b], idx) => compareItems(a,b) < 0 ? (idx + 1) : 0).sum());


    const dp1 = [[2]];
    const dp2 = [[6]];
    const all = [dp1, dp2].concat(...parsed);

    all.sort(compareItems);

    const dp1idx = all.indexOf(dp1) + 1;
    const dp2idx = all.indexOf(dp2) + 1;

    console.log('day13 b:', dp1idx * dp2idx);
    return all;
}

export function compareItems(a: number | Array<unknown>, b: number | Array<unknown>): number {
    if (typeof a === 'number' && typeof b === 'number') {
        return a - b;
    }
    if (Array.isArray(a) && Array.isArray(b)) {
        for (let i = 0; i < Math.min(a.length, b.length); i++) {
            const diff = compareItems(a[i], b[i]);
            if (diff !== 0) {
                return diff;
            }
        }
        return a.length - b.length;
    }
    if (Array.isArray(a)) {
        return compareItems(a, [b]);
    }
    else {
        return compareItems([a], b);
    }
}