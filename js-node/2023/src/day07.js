
import {strings as S, functions as F} from './utils.js'

export const sample = `
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`

const cards = "23456789TJQKA";
const cardsJ = "J23456789TQKA";

export const runA = (data) => parse(data)
  .map(({hand, bid}) => ({hand, bid, strength: handStrenth(typeStrength, card => cards.indexOf(card), hand)}))
  .sort(F.asc(F.key('strength')))
  .map(F.key('bid'))
  .map((v, i) => v * (i + 1))
  .sum();

export const runB = (data) => parse(data)
  .map(({hand, bid}) => ({hand, bid, strength: handStrenth(typeStrengthJ, card => cardsJ.indexOf(card), hand)}))
  .sort(F.asc(F.key('strength')))
  .map(F.key('bid'))
  .map((v, i) => v * (i + 1))
  .sum();


const handStrenth = (typeStrengthF, cardStrengthF, hand) => hand.split('').reverse().map(cardStrengthF).concat(typeStrengthF(hand)).map((v, i) => v * 100 ** i).sum();
const typeStrength = (hand) => hand.split('').sort().groupSame().map(F.snd).map(v => v*v).sum();
const typeStrengthJ = (hand) => {
  const grouped = hand.split('').sort().groupSame().sort(F.desc(F.snd))
  const idxJ = grouped.findIndex(([card]) => card === 'J');
  if (idxJ >= 0 && grouped.length > 1) {
    const [[_, jv]] = grouped.splice(idxJ, 1);
    grouped[0][1] += jv;
  }
  return grouped.map(F.snd).map(v => v*v).sum();
}

const parse = (str) => str.split('\n')
  .map(row => row.split(' '))
  .map(([hand, bid]) => ({hand, bid: S.asInt(bid)}))
