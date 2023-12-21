import {writeFileSync} from 'fs';
import {encode} from 'bmp-js'


/**
 * Save grid (2d array) as bmp image
 * @param {Object[][]} grid 
 * @param {string} fileName 
 * @param {(value: Object, col: number, row: number) => [number, number, number, number]} mapper gets cell value, should return array of 8bit (0-255) [A,R,G,B] values
 */
export const saveGridAsImage = (grid, fileName, mapper) => {
  const width = grid[0].length;
  const height = grid.length;
  const data = new Uint8Array(width * height * 4);

  for (let row = 0; row < height; ++row) {
    for (let col = 0; col < width; ++col) {
      const argb = mapper(grid[row]?.[col], col, row);
      for (let i = 0; i < 4; ++i) {
        data[row * width * 4 + col * 4 + i] = argb[i];
      }
    }
  }

  const raw = encode({width, height, data});
  writeFileSync(`./${fileName}.bmp`, raw.data);
}