import { asInt, lognb, repeat } from "#lib";
// %%


const sample = `2333133121414131402`
const data = await Deno.readTextFile('./data/day09.txt');

const parse = (s: string) => s.trim().split('').map(asInt);

parse(sample)

// %%

// solution of type "f*ck it, i have enough memory" xD
const solveA = (s: string) => {
  const nums = parse(s);

  const expanded: number[] = [];
  for (let i = 0; i < nums.length; ++i) {
    const count = nums[i];
    expanded.push(...repeat(i % 2 ? -1 : i / 2, count));
  }
  let i = 0, j = expanded.findLastIndex(n => n >= 0);
  while(i < j) {
    if (expanded[i] >= 0) {
      ++i;
    } else {
      expanded[i] = expanded[j];
      expanded[j] = -1;
      while(expanded[j] < 0) {
        --j;
      }
    }
  }

  return expanded.reduce((prev, val, idx) => val < 0 ? prev : prev + val * idx, 0);
}
solveA(sample)
// %%

console.log("Sol A:", solveA(data))
// %%

type Space = { len: number, isFile: boolean, id?: number };
const printExpanded = (nums: Space[]) => lognb(nums.flatMap(({len, isFile, id}) => repeat(isFile ? ''+id : '.', len)).join(''))

const solveB = (s: string) => {
  const nums: Space[] = parse(s).map((n, i) => i % 2 ? { len: n, isFile: false } : { len: n, isFile: true, id: i / 2 });
  // printExpanded(nums)
  let nextToMoveIdx = nums.findLastIndex(({isFile}) => isFile);
  let nextToMove = nums[nextToMoveIdx];
  while(nextToMove.id! > 0) {
    // lognb('next to move', nextToMove)
    const moveToIdx = nums.findIndex(({len, isFile}) => !isFile && len >= nextToMove.len);
    if (moveToIdx > 0 && moveToIdx < nextToMoveIdx) {
      nums[moveToIdx].len -= nextToMove.len;
      nums.splice(nextToMoveIdx, 1, {len: nextToMove.len, isFile: false});
      nums.insert(moveToIdx, nextToMove);
    }
    nextToMoveIdx = nums.findLastIndex(({isFile, id}) => isFile && id === nextToMove.id! - 1);
    nextToMove = nums[nextToMoveIdx];
    // printExpanded(nums)
  }

  let checksum = 0;
  let idx = 0;
  for (const sp of nums) {
    for (let i = 0; i < sp.len; ++i, ++idx) {
      if (sp.isFile) {
        checksum += idx * (sp.id ?? 0);
      }
    }
  }
  return checksum;
}
solveB(sample)

// %%

console.log('Sol B:', solveB(data))
