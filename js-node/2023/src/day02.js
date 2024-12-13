
import {strings as S} from './utils.js'

export const sample = `
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
`

export const parse = (str) => str.split('\n').map(parseGame);

const inBag = {red: 12, green: 13, blue: 14};

export const runA = (data) => parse(data)
  .filter(game => Object.entries(maxCubes(game.draws)).every(([color, num]) => inBag[color] >= num))
  .map(game => game.id)
  .sum();

export const runB = (data) => parse(data)
  .map(game => maxCubes(game.draws))
  .map(Object.values)
  .map(vals => vals.reduce((acc, num) => acc * num, 1))
  .sum();
  

function maxCubes(draws) {
  const pickMax = (target, source) => Object.assign(target, 
    Object.filter(source, ([color, num]) => (!target[color] || num > target[color])))
  return draws.reduce(pickMax, {});
}

function parseGame(str) {
  const [gameStr, drawsStr] = str.split(": ");
  const id = S.asInt(gameStr.split(' ')[1]);
  const draws = drawsStr.split('; ').map(draw => 
    draw.split(', ').map(colorDraw => {
      const [cubesCount, cubesColor] = colorDraw.split(' ');
      return {[cubesColor]: parseInt(cubesCount)}
    }).reduce((a,o) => ({...a, ...o}), {}));

  return {id, draws};
}
