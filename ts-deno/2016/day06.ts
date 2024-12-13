import { snd, desc, mapped, fst } from "#lib";
// %%

const sample = `
eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar`

const parse = (s: string): string[] => s.trim().split('\n');

parse(sample)
// %%

const getCounts = (lines: string[]): [string, number][][] => {
  const counts: Record<string, number>[] = Array.from({length: lines[0].length}, () => ({}));

  for (const line of lines) {
    for (let i = 0; i < line.length; ++i) {
      const c = line[i];
      const count = counts[i];
      count[c] = 1 + (count[c] ?? 0);
    }
  }

  return counts.map(count => Object.entries(count).sort(mapped(snd, desc)))
}

getCounts(parse(sample))

// %%

const solveA = (s: string) => getCounts(parse(s)).map(a => a[0]).map(fst).join('');

solveA(sample)
// %%

const data = await Deno.readTextFile('./data/day06.txt');

console.log("Sol A:", solveA(data))

// %%

const solveB = (s: string) => getCounts(parse(s)).map(a => a.at(-1)!).map(fst).join('');

console.log("Sol B:", solveB(data))
