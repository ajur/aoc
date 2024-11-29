import { asInt, transpose } from  './lib/index.ts';
// %%

const sample = `
5 10 25
 4 5 6
3 4 5`

type Triplet = number[];

const parse = (s: string): Triplet[] => s.trim().split('\n').map(l => l.trim().split(/\s+/).map(asInt) as Triplet)

parse(sample)
// %%

const canBeTriangle = ([a, b, c]: Triplet) => a < b + c && b < a + c && c < a + b;

const howManyPossible = (d: Triplet[]) => d.reduce((c: number, t: Triplet) => c + (canBeTriangle(t) ? 1 : 0), 0);

howManyPossible(parse(sample))

// %%

const data = await Deno.readTextFile('./data/day03.txt');
// %%

console.log("Sol A:", howManyPossible(parse(data)));

// %%

const parseVertically = (s: string): Triplet[] => transpose(parse(s)).flat().inGroupOf(3)

console.log("Sol B:", howManyPossible(parseVertically(data)));
