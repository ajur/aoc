
export function day10s(day10sample: string) {
    day10(day10sample);
}

export function day10(data: string) {
    const parsed = data.trim().split('\n');

    const regLog = buildRegLog(parsed);

    const poi = Array.from({ length: 6 }, (v, i) => i * 40 + 20);
    console.log('day10 a', poi.map(i => i * regLog[i - 1]).sum());

    console.log('day19 b');
    console.log(renderCRTImage(regLog));
}
function buildRegLog(instructions: string[]) {
    let x = 1;
    const regLog = [x];
    for (const instruction of instructions) {
        const [expr, val] = instruction.split(' ');
        if (expr === 'noop') {
            regLog.push(x);
        }
        else if (expr === 'addx') {
            regLog.push(x);
            x += parseInt(val, 10);
            regLog.push(x);
        }
    }
    return regLog;
}
function renderCRTImage(regLog: number[]) {
    const sw = 40;
    const sh = 6;
    const screen = Array.from({ length: sh * sw }, () => 0);

    for (let i = 0; i < sw * sh; i++) {
        const x = regLog[i];
        const sx = i % sw;
        if (Math.abs(sx - x) <= 1) {
            screen[i] = 1;
        }
    }

    return screen.inGroupOf(sw).map(r => r.map(i => i ? '#' : '.').join('')).join('\n');
}
