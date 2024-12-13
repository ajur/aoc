import { md5 } from "jsr:@takker/md5";
import { eq } from "#lib";

// %%

const solveA = (s: string): string => {
  let i = 0;
  const out: string[] = [];
  while (out.length < 8) {
    const hash = md5(s + i);
    const view = new Uint8Array(hash);
    if (view[0] === 0 && view[1] === 0 && view[2] < 16) {
      out.push(view[2].toString(16));
      console.log(`found next at ${i}, out is ${out}`);
    }
    ++i;
  }
  return out.join('');
}

// test
solveA('abc');

// %%

console.log("Sol A:", solveA('ugkcyxxp'))

// %%

const solveB = (s: string): string => {
  let i = 0;
  const out = Array.from('________');
  while (true) {
    const hash = md5(s + i);
    const view = new Uint8Array(hash);
    if (view[0] === 0 && view[1] === 0 && view[2] < 8 && out[view[2]] === '_') {
      const s = view[3].toString(16);
      out[view[2]] = s.length === 1 ? '0' : s[0];
      console.log(`found next at ${i}, out is ${out}`);
      if (out.findIndex(eq('_')) < 0) break;
    }
    ++i;
  }
  return out.join('');
}

// test
solveB('abc');

// %%

console.log("Sol B:", solveB('ugkcyxxp'));
