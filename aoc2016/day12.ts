// %%

import {  } from "./lib/index.ts";


const sample = `
cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a`;

type Instruction = [string, Array<string|number>]

const parseInstruction = (s: string): Instruction => {
  const [instr, ...params] = s.split(' ');
  return [instr, params.map(p => Number.isInteger(+p) ? +p : p)]
}

const parse = (s: string): Instruction[] => s.trim().split('\n').map(parseInstruction);
parse(sample)
// %%

const compute = (prog: Instruction[], register: Record<string, number>): number => {
  const asValue = (a: string | number): number => typeof a === "number" ? a : register[a];

  let idx = 0;

  while (idx < prog.length) {
    const [instr, [x, y]] = prog[idx];
    switch(instr) {
      case 'inc':
        ++register[x];
        ++idx;
        break;
      case 'dec':
        --register[x];
        ++idx;
        break;
      case 'cpy':
        register[y] = asValue(x);
        ++idx;
        break;
      case 'jnz':
        if (asValue(x) !== 0) {
          idx += asValue(y);
        } else {
          ++idx;
        }
        break;
    }
  }
  return register.a;
}

compute(parse(sample), { a: 0, b: 0, c: 0, d: 0 })

// %%

const data = `
cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 14 c
cpy 14 d
inc a
dec d
jnz d -2
dec c
jnz c -5`

console.log('Sol A:', compute(parse(data),  { a: 0, b: 0, c: 0, d: 0 }));

// %%

console.log('Sol B:', compute(parse(data),  { a: 0, b: 0, c: 1, d: 0 }));
