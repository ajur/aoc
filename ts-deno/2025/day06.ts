import { lognb, asInt, transpose } from '#lib';
// %%

const sample =`
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
`;

type ParsedData = [...number[], string][];
type Parser = (s: string) => ParsedData;

const parseA: Parser = (s: string) => transpose(
  s.trim()
    .split('\n')
    .map(l => l.trim()
      .split(/\s+/)
      .map(n => (n[0] !== '*' && n[0] !== '+') ? asInt(n) : n)
      )) as ParsedData;

lognb(parseA(sample))

// %%

const solveA = (ops: ParsedData) => ops
  .map(dat => {
    const op = dat.pop()! as string;
    let m = dat[0] as number;
    if (op === '+') {
      for (let i = 1; i < dat.length; ++i) {
        m += (dat[i] as number);
      }
    } else {
      for (let i = 1; i < dat.length; ++i) {
        m *= (dat[i] as number);
      }
    }
    return m;
  })
  .sum();

solveA(parseA(sample));

// %%

const data = await Deno.readTextFile("./data/day06.txt");

console.log("Sol A:", solveA(parseA(data)))

// %%

const parseB = (s: string) => transpose(s.trim('\n').split('\n').map(l => l.split('')))
  .map(l => l.map(c => !c || c === ' ' ? '' : c));

parseB(sample);

// %%
const solveB = (data: string[][]) => {
  let total = 0;
  let local = 0;
  let op = null;
  for (const l of data) {
    const nop = l.pop();
    const ns = l.join('');
    if (ns.length === 0) continue;
    const n = asInt(ns);
    if (nop) {
      if (op) {
        total += local;
      }
      op = nop;
      local = n;
    } else {
      if (op === '+') {
        local += n;
      } else {
        local *= n;
      }
    }
  }
  total += local;
  return total;
}

solveB(parseB(sample));
// %%

console.log("Sol B:", solveB(parseB(data)))
