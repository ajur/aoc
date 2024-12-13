
const day3sample = `
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
`;


export function day03(data: string) {
    const parsed = data.trim().split('\n');

    console.log('day3 a:', parsed.map(findDupInLine).sum());

    console.log('day3 b:', parsed.map(l => Array.from(l)).inGroupOf(3).map(findCommonInLines).map(c => charcodeToValue(c.charCodeAt(0))).sum());
}

export function day03s() {
    day03(day3sample);
}

function charcodeToValue(charcode: number) {
    if (charcode > 96 && charcode < 123) {
        return charcode - 96;
    }
    if (charcode > 64 && charcode < 91) {
        return charcode - 38;
    }
    return 0;
}
function findDupInLine(line: string) {
    const chars = new Set();
    for (let i = 0; i < line.length / 2; i++) {
        chars.add(charcodeToValue(line.charCodeAt(i)));
    }
    for (let i = line.length / 2; i < line.length; i++) {
        const code = charcodeToValue(line.charCodeAt(i));
        if (chars.has(code)) {
            return code;
        }
    }
}
function findCommonInLines(lines: string[][]) {
    const last = lines.pop();
    const intersection = lines.reduce((prev, line) => prev.intersection(line), last).unique();
    return intersection[0];
}
