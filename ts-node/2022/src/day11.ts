

import './toolbelt';

const day11sample = `
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
`;

export function day11s() {
    return day11(day11sample);
}

export function day11(data: string) {
    const monkeys = data.trim().split('\n\n').map(spec => parseMonkey(spec));
    monkeys.forEach(monkey => monkey.setAllMonkeysRef(monkeys));

    for (let i = 0; i < 20; ++i) {
        monkeys.forEach(monkey => monkey.inspectAndThrow());
    }
    const topInspected = monkeys.map(m => m.itemsInspected).sortDesc();

    console.log('day11 a:', topInspected[0] * topInspected[1]);



    const worringMonkeys = data.trim().split('\n\n').map(spec => parseMonkey(spec, true));
    worringMonkeys.forEach(monkey => monkey.setAllMonkeysRef(worringMonkeys));

    for (let i = 0; i < 10000; ++i) {
        // if (i % 100 === 0) console.log('iteration', i, '\n', worringMonkeys);
        for (let j = 0; j < worringMonkeys.length; ++j) {
            worringMonkeys[j].inspectAndThrow();
        }
    }
    const topWoriedInspected = worringMonkeys.map(m => m.itemsInspected).sortDesc();

    console.log('day11 b:', topWoriedInspected[0] * topWoriedInspected[1]);

    return worringMonkeys;
}

function parseMonkey(data: string, isInsepctionWorring = false) {
    const id = data.match(/Monkey (\d+):/)[1].toInt();
    const items = data.match(/Starting items:((?: (?:\d+),?)*)/)[1].trim().split(', ').map(s => s.toInt());
    const opBody = data.match(/Operation: new = ([^\n]+)/)[1];
    const op = new Function('old', 'return ' + opBody) as OperationFn;
    const divisableBy = data.match(/Test: divisible by (\d+)/)[1].toInt();
    const ifTrue = data.match(/If true: throw to monkey (\d+)/)[1].toInt();
    const ifFalse = data.match(/If false: throw to monkey (\d+)/)[1].toInt();

    return new Monkey(id, items, op, divisableBy, ifTrue, ifFalse, isInsepctionWorring);
}

type OperationFn = (old: number) => number;
type ThrowSpec = { item: number, to: number };

class Monkey {

    itemsInspected = 0;
    allMonkeys: Monkey[];
    divisibleCap: number;

    constructor(
        public id: number,
        public items: number[],
        public op: OperationFn,
        public divisableBy: number,
        public ifTrue: number,
        public ifFalse: number,
        public isInsepctionWorring
    ) {}

    public setAllMonkeysRef(monkeys: Monkey[]) {
        for (var i = 0; i < monkeys.length; i++) {
            if (monkeys[i].id !== i) throw new Error(`Monkey idx [${i}] missmatch its id [${monkeys[i].id}]`);
        }
        this.allMonkeys = monkeys;
        this.divisibleCap = monkeys.map(monkey => monkey.divisableBy).reduce((a,b) => a * b, 1);
    }

    public catch(item: number) {
        this.items.push(item);
    }

    public inspectAndThrow() {
        for (const item of this.items) {
            this.inspectNext(item);
        }
        this.items = [];
    }

    inspectNext(item: number) {
        ++this.itemsInspected;

        let next = this.op(item);

        if (this.isInsepctionWorring) {
            next = next % this.divisibleCap;
        } else {
            next = Math.floor(next / 3);
        }
        
        this.allMonkeys[(next % this.divisableBy === 0) ? this.ifTrue : this.ifFalse].catch(next);
    }
}
