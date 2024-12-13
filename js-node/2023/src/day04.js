
import './utils.js'

export const sample = `
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
`

export const runA = (data) => parse(data)
  // .map(row => console.log(row) || row)
  .map(({wins, owns}) => wins.intersection(owns).length)
  .filter(n => n > 0)
  .map(n => 2 ** (n-1))
  .sum()

export const runB = (data) => parse(data)
  .map(({id, wins, owns}) => ({id, wins: wins.intersection(owns).length, copies: 1}))
  .reduce((cards, {id, wins, copies}) => {
    if (!cards[id]) cards[id] = 1;
    // console.log("#", id, 'w', wins, 'cp', copies, 'agg', cards)
    for (let i = 1; i <= wins; ++i) {
      cards[id + i] = (cards[id + i] || 1) + cards[id];
    }
    // console.log(cards)
    return cards
  }, [0])
  .sum()


const parse = (str) => str.split('\n')
  .map(row => row.match(/Card\s+(\d+):\s+([\s\d]+) \|\s+([\s\d]+)/))
  .map(([_, cardId, wins, owns]) => ({
    id: parseInt(cardId), 
    wins: wins.split(/\s+/).map(n => parseInt(n)), 
    owns: owns.split(/\s+/).map(n => parseInt(n))
  }));
