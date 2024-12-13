
const day2sample = `
A Y
B X
C Z
`;

export function day02s() {
    day02(day2sample);
}
type RPS = 'Rock' | 'Paper' | 'Scissors';
type RPSO = 'Win' | 'Draw' | 'Loss';
const basePoints = {
    Rock: 1,
    Paper: 2,
    Scissors: 3
};
const winPoints = {
    Win: 6,
    Draw: 3,
    Loss: 0
};
function winOrLoss(opponent: RPS, me: RPS): RPSO {
    switch (opponent) {
        case 'Rock':
            switch (me) {
                case 'Paper': return 'Win';
                case 'Scissors': return 'Loss';
                case 'Rock': return 'Draw';
            }
        case 'Paper':
            switch (me) {
                case 'Scissors': return 'Win';
                case 'Rock': return 'Loss';
                case 'Paper': return 'Draw';
            }
        case 'Scissors':
            switch (me) {
                case 'Rock': return 'Win';
                case 'Paper': return 'Loss';
                case 'Scissors': return 'Draw';
            }
    }
}
function roundPoints(opponent: RPS, me: RPS): number {
    return winPoints[winOrLoss(opponent, me)] + basePoints[me];
}
function oponentParser(val: string): RPS {
    switch (val) {
        case 'A': return 'Rock';
        case 'B': return 'Paper';
        case 'C': return 'Scissors';
        default: throw 'Unsupported RPC value ' + val;
    }
}
function meParser(val: string): RPS {
    switch (val) {
        case 'X': return 'Rock';
        case 'Y': return 'Paper';
        case 'Z': return 'Scissors';
        default: throw 'Unsupported RPC value ' + val;
    }
}
function meParser_outcome(opponent: RPS, val: string): RPS {
    switch (val) {
        case 'X': return opponent === 'Rock' ? 'Scissors' : opponent === 'Paper' ? 'Rock' : 'Paper';
        case 'Y': return opponent === 'Rock' ? 'Rock' : opponent === 'Paper' ? 'Paper' : 'Scissors';
        case 'Z': return opponent === 'Rock' ? 'Paper' : opponent === 'Paper' ? 'Scissors' : 'Rock';
        default: throw 'Unsupported RPC value ' + val;
    }
}

export function day02(data: string) {
    const parsed = data.trim()
        .split('\n')
        .map((line) => line
            .split(' ')
        );

    const points_a = parsed.map(p => roundPoints(oponentParser(p[0]), meParser(p[1])));
    const sum_a = points_a.reduce((a, b) => a + b, 0);
    console.log('day2 a: ' + sum_a);

    const points_b = parsed.map(p => {
        const opp = oponentParser(p[0]);
        const me = meParser_outcome(opp, p[1]);
        return roundPoints(opp, me);
    });
    const sum_b = points_b.reduce((a, b) => a + b, 0);
    console.log('day2 a: ' + sum_b);
}
