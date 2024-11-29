


export const transpose = <T>(g: T[][]): T[][] => {
  const rows = g.length;
  const cols = g[0].length;
  const out = new Array(cols);
  for (let col = 0; col < cols; ++col) {
    out[col] = new Array(rows);
    for (let row = 0; row < rows; ++row) {
      out[col][row] = g[row][col];
    }
  }
  return out;
};
