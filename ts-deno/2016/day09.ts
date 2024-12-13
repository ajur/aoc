// %%

import { assertEquals } from "@std/assert";
import { asInt } from "#lib";

const decompressedLength = (s: string) => {
  let i = 0;
  let len = 0;
  while (i < s.length) {
    const openMarker = s.indexOf('(', i);
    if (openMarker < 0) {
      len += s.length - i;
      i = s.length;
    } else {
      len += openMarker - i;
      const closeMarker = s.indexOf(')', openMarker);
      if (closeMarker < 0) throw new Error("no matching parenthases on " + i);
      const [rc, rn] = s.slice(openMarker + 1, closeMarker).split('x').map(asInt) as [number, number];
      len += rc * rn;
      i = closeMarker + 1 + rc;
    }
  }
  return len;
}

const samples = [
  'ADVENT',
  'A(1x5)BC',
  '(3x3)XYZ',
  'A(2x2)BCD(2x2)EFG',
  '(6x1)(1x3)A',
  'X(8x2)(3x3)ABCY',
  '(2x12)AA'
];
const expectedLengths = [6, 7, 9, 11, 6, 18, 24];

assertEquals(samples.map(decompressedLength), expectedLengths)

// %%

const data = await Deno.readTextFile('./data/day09.txt');

console.log('Sol A:', decompressedLength(data.trim()));

// %%

const decompressedLengthV2 = (s: string) => {
  let i = 0;
  let len = 0;
  while (i < s.length) {
    const openMarker = s.indexOf('(', i);
    if (openMarker < 0) {
      len += s.length - i;
      i = s.length;
    } else {
      len += openMarker - i;
      const closeMarker = s.indexOf(')', openMarker);
      if (closeMarker < 0) throw new Error("no matching parenthases on " + i);
      const [rc, rn] = s.slice(openMarker + 1, closeMarker).split('x').map(asInt) as [number, number];
      i = closeMarker + 1 + rc;
      const subLen = decompressedLengthV2(s.slice(closeMarker + 1, i));
      len += subLen * rn;
    }
  }
  return len;
}

const samples2 = [
  '(3x3)XYZ',
  'X(8x2)(3x3)ABCY',
  '(27x12)(20x12)(13x14)(7x10)(1x12)A',
  '(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN',
];
const expectedLengths2 = [9, 20, 241920, 445];

assertEquals(samples2.map(decompressedLengthV2), expectedLengths2)

// %%

console.log("Sol B:", decompressedLengthV2(data.trim()))
