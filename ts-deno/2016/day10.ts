// %%

import { asc, asInt } from  "#lib";

// %%

const sample = `
value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2`;

type Target = ['bot' | 'output', number];
type InitIntruction = {
  op: 'init';
  bot: number;
  val: number;
};
type BotInstruction = {
  op: 'bot',
  bot: number,
  low: Target,
  high: Target,
}
type Instruction = InitIntruction | BotInstruction;

const parseInstructions = (s: string): Instruction => {
  const tokens = s.split(' ');
  if (tokens[0] === 'value') {
    return { op: 'init', val: asInt(tokens[1]), bot: asInt(tokens[5]) };
  } else if (tokens[0] === 'bot') {
    if (tokens[3] !== 'low') throw new Error("unexpected instruction layout: " + s);
    return {
      op: 'bot',
      bot: asInt(tokens[1]),
      low: [tokens[5], asInt(tokens[6])] as Target,
      high: [tokens[10], asInt(tokens[11])] as Target,
    }
  }
  throw new Error("parsing error on line " + s);
}

const parse = (s: string): Instruction[] => s.trim().split('\n').map(parseInstructions);

parse(sample);
// %%

type Bot = {
  chips: number[],
  exec?: (watchFor?: [number, number]) => void,
}
type Output = {
  chips: number[],
}

type State = {
  bot: Bot[],
  output: Output[],
}

const passChip = ([t, tId]: Target, v: number, state: State) => {
  state[t][tId] ??= { chips: [] };
  state[t][tId].chips.push(v);
}

const prepExec = ({low, high}: BotInstruction, state: State) => function(this: Bot, watchFor?: [number, number]) {
  if (this.chips.length >= 2) {
    const [lowChip, highChip] = this.chips.splice(0, 2).sort(asc);
    passChip(low, lowChip, state);
    passChip(high, highChip, state);
    if (watchFor?.[0] === lowChip && watchFor?.[1] === highChip) return true;
  }
  return false;
}

const setupState = (insts: Instruction[]) => {
  const state: State = {
    bot: [],
    output: [],
  };

  for (const inst of insts) {
    if (inst.op === 'init') {
      state.bot[inst.bot] ??= { chips: [] };
      state.bot[inst.bot].chips.push(inst.val);
    } else {
      state.bot[inst.bot] ??= { chips: [] };
      state.bot[inst.bot].exec = prepExec(inst, state);
    }
  }

  return state;
}
setupState(parse(sample))

// %%

const solveA = (s: string, watchFor: [number, number]) => {
  const state = setupState(parse(s));

  while (true) {
    for (let i = 0; i < state.bot.length; ++i) {
      const found = state.bot[i]?.exec?.(watchFor);
      if (found) {
        return i;
      }
    }
  }
}

console.log("Sol A:", solveA(sample, [2, 5]))

// %%

const data = await Deno.readTextFile('./data/day10.txt');

solveA(data, [17, 61])

// %%

const checkOutputs = (watchFor: number[], state: State): number => {
  const outputs = watchFor.map((id) => state.output[id]);
  let sol = 1;
  for (const output of outputs) {
    if (output?.chips?.length > 0) {
      sol *= output.chips[0];
    } else {
      return 0;
    }
  }
  return sol;
}

const solveB = (s: string, watchFor: number[]) => {
  const state = setupState(parse(s));

  while (true) {
    for (let i = 0; i < state.bot.length; ++i) {
      state.bot[i]?.exec?.();

      const out = checkOutputs(watchFor, state);
      if (out !== 0) {
        return out;
      }
    }
  }
}

console.log("Sol B:", solveB(data, [0, 1, 2]))
