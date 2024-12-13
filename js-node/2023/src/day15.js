

import {strings as S} from './utils.js'

export const sample = `
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
`

export const runA = (data) => parse(data)
  .map(hash)
  .sum()

export const runB = (data) => parse(data)
  .reduce(hashmap, [])
  .flatMap((box, boxIdx) => box.map(([lbl, lf], slotIdx) => (boxIdx + 1) * (slotIdx + 1) * lf ))
  .sum()

const hashmap = (boxMap, instr) => interpret(instr)(boxMap)

const interpret = (str) => {
  if (str.indexOf('=') > 0) {
    const [label, focal] = str.split('=');
    return addToBox(label, S.asInt(focal))
  } else {
    return removeFromBox(str.substring(0, str.length - 1));
  }
}

const addToBox = (label, focal) => boxMap => {
  const lh = hash(label);
  if (!boxMap[lh]) {
    boxMap[lh] = [[label, focal]];
    return boxMap;
  }
  const foundAt = boxMap[lh].findIndex(([lb]) => lb == label);
  if (foundAt < 0) {
    boxMap[lh].push([label, focal]);
  } else {
    boxMap[lh][foundAt][1] = focal;
  }
  return boxMap;
}

const removeFromBox = (label) => boxMap => {
  const lh = hash(label);
  if (!boxMap[lh]) return boxMap;
  const foundAt = boxMap[lh].findIndex(([lb]) => lb == label);
  if (foundAt >= 0) {
    boxMap[lh].splice(foundAt, 1);
  }
  return boxMap;
}


const hash = (str) => {
  let curr = 0;
  for (let i = 0; i < str.length; ++i) {
    curr += str.charCodeAt(i);
    curr *= 17;
    curr %= 256;
  }
  return curr;
}


const parse = (str) => str.split(',')
  