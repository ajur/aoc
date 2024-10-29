import {writeFileSync} from 'fs';
import {encode} from 'bmp-js'


/**
 * mapper from integer number to list RGBA values
 * @param {number} num integer value in range 0x000000 to 0xFFFFFF 
 * @returns [number, number, number, number] list of 8bit RGBA values, with A being allways 0xFF
 */
export const intToRGBA = (num) => ([
  (num & 0xFF0000) >>> 16,
  (num & 0xFF00) >>> 8,
  (num & 0xFF),
  0xFF
]);

/**
 * Save grid (2d array) as bmp image
 * @param {Object[][]} grid 
 * @param {string} fileName 
 * @param {(value: Object, col: number, row: number) => [number, number, number, number]} mapper gets cell value, should return array of 8bit (0-255) [R, G, B, A] values
 */
export const saveGridAsImage = (grid, fileName, mapper = intToRGBA) => {
  const width = grid[0].length;
  const height = grid.length;
  const data = new Uint8Array(width * height * 4);

  for (let row = 0; row < height; ++row) {
    for (let col = 0; col < width; ++col) {
      const abgr = mapper(grid[row]?.[col], col, row);
      for (let i = 0; i < 4; ++i) {
        data[row * width * 4 + col * 4 + i] = abgr[3 - i];
      }
    }
  }

  const raw = encode({width, height, data});
  writeFileSync(`./viz/${fileName}.bmp`, raw.data);
}
