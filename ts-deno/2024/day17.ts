// %%
import { assertEquals } from "@std/assert";
import { log, ansi, isJupyter } from "#lib";
// %%

const sample = `
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
`

const data = `
Register A: 21539243
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,1,5,0,3,4,1,5,5,3,0
`
type State = { a: bigint, b: bigint, c: bigint, p: bigint[], i: number, out: bigint[] };

const parse = (s: string) => {
  const [ra, rb, rc, _, pr] = s.trim().split('\n');
  return {
    a: BigInt(ra.split(': ')[1]),
    b: BigInt(rb.split(': ')[1]),
    c: BigInt(rc.split(': ')[1]),
    p: pr.split(': ')[1].split(',').map(BigInt),
    i: 0,
    out: [],
  } as State;
}
parse(sample)
// %%

const literal = (st: {p: bigint[], i: number}) => st.p[st.i + 1];

const combo = <T>(st: {p: bigint[], i: number, a: T, b: T, c: T}) => {
  const v = st.p[st.i + 1];
  if (v <= 3) return v;
  switch (v) {
    case 4n: return st.a;
    case 5n: return st.b;
    case 6n: return st.c;
    default: throw Error("invalid combo operand"+v);
  }
}

const adv = (st: State) => {  // 0
  st.a = st.a >> combo(st);
  st.i += 2;
}
const bxl = (st: State) => {  // 1
  st.b = st.b ^ literal(st);
  st.i += 2;
}
const bst = (st: State) => {  // 2
  st.b = combo(st) & 7n;
  st.i += 2;
}
const jnz = (st: State) => {  // 3
  if (st.a > 0) {
    st.i = Number(literal(st));
  } else {
    st.i += 2;
  }
}
const bxc = (st: State) => {  // 4
  st.b = st.b ^ st.c;
  st.i += 2;
}
const out = (st: State) => {  // 5
  st.out.push(combo(st) & 7n);
  st.i += 2;
}
const bdv = (st: State) => {  // 6
  st.b = st.a >> combo(st);
  st.i += 2;
}
const cdv = (st: State) => {  // 7
  st.c = st.a >> combo(st);
  st.i += 2;
}

const byOpcode = [adv, bxl, bst, jnz, bxc, out, bdv, cdv];

const execute = (st: State) => {
  while(st.i < st.p.length - 1) {
    byOpcode[Number(st.p[st.i])](st);
  }
  return st;
}

const solveA = (s: string) => execute(parse(s)).out.join(',');
assertEquals(solveA(sample), '4,6,3,5,6,3,5,2,1,0');

// %%
console.log('Sol A:', solveA(data));

// %%

const sample2 = `
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0`

// %%

const testSt = (state: State, a: bigint) => {
  const st: State = {...state, a: a, out: []};
  // log('looking for', st.p.join(','));
  while(st.i < st.p.length - 1) {
    const opId = Number(st.p[st.i]);
    byOpcode[opId](st);
    if (opId === 5 && st.out.length <= st.p.length) { // out
      // if (st.out.length > st.p.length) return false;
      if (st.out.at(-1) !== st.p[st.out.length - 1]) return false;
      // if (st.out.length > 4) log("out for", a, "->", st.out.join(','));
    }
  }
  // log("finished at", st.out.join(','));
  // if (st.out.length !== st.p.length) return false;
  for (let i = 0; i < st.out.length, i < st.p.length; ++i) {
    if (st.out[i] !== st.p[i]) return false;
  }
  // log('true on state', JSON.stringify(st));
  return true;
}

assertEquals(testSt(parse(sample2), 117440n), true);

// %%

const bruteSolveB = (s: string, n?: bigint) => {
  const st = parse(s);
  let a = n ?? 0n;
  log(a);
  while (!testSt(st, a)) {
    ++a;
    log(ansi.cpl() + a);
  }
  return a;
}

assertEquals(bruteSolveB(sample2, 100000n), 117440n);
// not worth running on prod data -_-
// %%

// Parts of some analyzis...

type StateA = { a: string, b: string, c: string, p: bigint[], i: number, out: string[] };


const advA = (st: StateA) => {
  st.a = `(${st.a} >> ${combo(st)})`;
  st.i += 2;
}
const bxlA = (st: StateA) => {
  st.b = `(${st.b} ^ ${literal(st)})`;
  st.i += 2;
}
const bstA = (st: StateA) => {
  st.b = `(${combo(st)} & 7)`;
  st.i += 2;
}
const jnzA = (st: StateA) => {
  if (st.out.length < st.p.length) {
    st.i = Number(literal(st));
  } else {
    st.out.push(`${st.a} === 0`)
    st.i += 2;
  }
}
const bxcA = (st: StateA) => {
  st.b = `(${st.b} ^ ${st.c})`;
  st.i += 2;
}
const outA = (st: StateA) => {
  st.out.push(`(${combo(st)} & 7) === ${st.p[st.out.length]}`);
  st.i += 2;
}
const bdvA = (st: StateA) => {
  st.b = `(${st.a} >> ${combo(st)})`;
  st.i += 2;
}
const cdvA = (st: StateA) => {
  st.c = `(${st.a} >> ${combo(st)})`;
  st.i += 2;
}

const byOpcodeA = [advA, bxlA, bstA, jnzA, bxcA, outA, bdvA, cdvA];

const anSolveB = (s: string) => {
  const st: StateA = {...parse(s), a: 'A', b: 'B', c: 'C', out: []};
  while(st.i < st.p.length - 1) {
    if (st.i === 0) {
      log(st);
    }
    byOpcodeA[Number(st.p[st.i])](st);
  }
  log(st.out.join('\n'));
};
if (isJupyter) anSolveB(sample2);
if (Deno.args.indexOf("-vv") >= 0) anSolveB(data);



// %%


// checking outputs, we know that e.g. this find first 6 numbers... 1138285 -> 2,4,1,3,7,5
//
// and that formula for each output is:
// OUT = ((((A & 7) ^ 3) ^ 5) ^ (A >> ((A & 7) ^ 3))) & 7
// next A = (A >> 3)
//
// so each for each output, we shift our number, and check for next output from reversed

// some checks by hand
{
  const out = (A: bigint) => ((((A & 7n) ^ 3n) ^ 5n) ^ (A >> ((A & 7n) ^ 3n))) & 7n;
  const next = (A: bigint) => (A >> 3n);

  const AS = "0b000_110_001_001"
  //                110_001_001
  //                    001_101
  //                    101_001
  //                    101_101
  //                    101_110
  const A = BigInt(AS.replaceAll('_', ''));
  const B = (n: number | bigint): string => n.toString(2).padStart(AS.replaceAll('_', '').length + 1, '0').split('').inGroupOf(3).map(g=>g.join('')).join('_');

  log(B(A), "A", A)
  log(B(A & 7n), "(A & 111)", (A & 7n))
  log(B((A & 7n) ^ 3n), "((A & 111) ^ 011)", ((A & 7n) ^ 3n))
  log(B(((A & 7n) ^ 3n) ^ 5n), "B = (((A & 111) ^ 011) ^ 101)", (((A & 7n) ^ 3n) ^ 5n))
  log(B(A >> ((A & 7n) ^ 3n)), "C = (A >> ((A & 111) ^ 011))", (A >> ((A & 7n) ^ 3n)))
  log(B((((A & 7n) ^ 3n) ^ 5n) ^ (A >> ((A & 7n) ^ 3n))), "B ^ C", ((((A & 7n) ^ 3n) ^ 5n) ^ (A >> ((A & 7n) ^ 3n))))
  log(B(out(A)), "out", out(A))
  log(B(A >> 3n), "next (A >> 3)", (A >> 3n))

  const lookingFor = [2,4,1,3,7,5,1,5,0,3,4,1,5,5,3,0].map(BigInt);
  log('looking for')
  log(lookingFor.join(','));
  for (let k = 0; k < 8; ++k) {
    const outs = [];
    let a = (A << 3n) | BigInt(k);
    while (a > 0) {
      outs.push(out(a))
      a = next(a);
    }
    log(outs.map(Number).join(',').padStart(lookingFor.join(',').length), B((A << 3n) | BigInt(k)))
  }
}

// %%

const solveBforData = () => {
  const out = (A: bigint) => ((((A & 7n) ^ 3n) ^ 5n) ^ (A >> ((A & 7n) ^ 3n))) & 7n;
  const next = (A: bigint) => (A >> 3n);
  const B = (n: number | bigint): string => n.toString(2).split('').inGroupOf(3).map(g=>g.join('')).join('_');

  const lookingFor = [2,4,1,3,7,5,1,5,0,3,4,1,5,5,3,0].map(BigInt);

  log('looking for')
  log(lookingFor.join(','));

  const findNext = (A: bigint, j: number): bigint => {
    for (let k = 0n; k < 8n; ++k) {
      const a = (A << 3n) | k;
      const o = out(a);
      if (o === lookingFor[j]) {
        const outs = [];
        let aa = a;
        while (aa > 0) {
          outs.push(out(aa))
          aa = next(aa);
        }
        log(outs.map(Number).join(',').padStart(lookingFor.join(',').length), B((A << 3n) | BigInt(k)))
        if (j === 0) return a;
        const oo = findNext(a, j - 1);
        if (oo > 0) {
          return oo;
        }
      }
    }
    return 0n;
  }
  return findNext(BigInt('0'), lookingFor.length - 1)
}
console.log('Sol B:', solveBforData());
