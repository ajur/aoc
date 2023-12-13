
import './toolbelt';

export function day01(data: string) {
    const parsed = data.trim()
        .split('\n\n')
        .map((line) => line
            .split('\n')
            .map(val => parseInt(val, 10))
            .reduce((acc, line) => acc + line, 0));
    parsed.sort((a, b) => b - a);
    console.log('day1 a: ' + parsed[0]);
    console.log('day1 b: ' + (parsed[0] + parsed[1] + parsed[2]));
}
