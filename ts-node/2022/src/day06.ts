
const day06sample = `
mjqjpqmgbljsphdztnvjfqwrcgsmlb 7 19
bvwbjplbgvbhsrlpgdmjqwftvncz 5 23
nppdvjthqldpwncqszvftbrmjlhg 6 23
nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg 10 29
zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw 11 26
`;

export function day06s() {
    day06sample.trim().split('\n').forEach(sample => {
        const [data, result_a, result_b] = sample.split(' ');
        console.log('-- sample:', data);
        console.log('-- expected: a', result_a, 'b', result_b);
        day06(data);
    });
}


export function day06(data: string) {
    const parsed = data.trim();

    const distinct4number = findDistinct4Index(parsed);
    console.log('day06 a:', distinct4number + 1);
    console.log('day06 b:', findDistinct14Index(parsed, distinct4number - 3) + 1);
}
function findDistinct4Index(parsed: string): number {
    for (let i = 3; i < parsed.length; i++) {
        if (parsed[i] !== parsed[i - 1] && parsed[i] !== parsed[i - 2] && parsed[i] !== parsed[i - 3] &&
            parsed[i - 1] !== parsed[i - 2] && parsed[i - 1] != parsed[i - 3] &&
            parsed[i - 2] !== parsed[i - 3]) {
            return i;
        }
    }
    return -1;
}
function findDistinct14Index(parsed: string, startAt: number): number {
    let i = startAt;
    const counts = new Map<string, number>();
    for (; i < startAt + 14; i++) {
        const char = parsed[i];
        counts.set(char, (counts.get(char) || 0) + 1);
    }
    if (counts.size == 14) {
        return i - 1;
    }
    for (; i < parsed.length; i++) {
        const char = parsed[i];
        counts.set(char, (counts.get(char) || 0) + 1);
        const toRemove = parsed[i - 14];
        if (counts.get(toRemove) > 1) {
            counts.set(toRemove, counts.get(toRemove) - 1);
        }
        else {
            counts.delete(toRemove);
        }
        if (counts.size == 14) {
            return i;
        }
    }
    return -1;
}
