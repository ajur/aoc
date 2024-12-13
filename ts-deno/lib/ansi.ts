// All codes base on https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit

/** ANSI Escape character */
export const ESCAPE = '\x1b';
/** ANSI Control Sequence Introducer */
export const CSI = '\x1b[';

/** Moves the cursor n (default 1) cells up. */
export const cuu = (n = 1) => `${CSI}${n}A`;

/** Moves the cursor n (default 1) cells down. */
export const cud = (n = 1) => `${CSI}${n}B`;

/** Moves the cursor n (default 1) cells forward. */
export const cuf = (n = 1) => `${CSI}${n}C`;

/** Moves the cursor n (default 1) cells back. */
export const cub = (n = 1) => `${CSI}${n}D`;

/** Moves cursor to beginning of the line n (default 1) lines down. */
export const cnl = (n = 1) => `${CSI}${n}E`;

/** Moves cursor to beginning of the line n (default 1) lines up. */
export const cpl = (n = 1) => `${CSI}${n}F`;

/** Moves the cursor to column n (default 1). */
export const cha = (n = 1) => `${CSI}${n}G`;

/** Moves the cursor to row n, column m.
 * Defaults to top left corner if omitted.
 */
export const cup = (n = 1, m = 1) => `${CSI}${n};${m}H`;

/** Clears part of the screen based on n.
 * If n is 0 (or missing), clear from cursor to end of screen.
 * If n is 1, clear from cursor to beginning of the screen.
 * If n is 2, clear entire screen.
 * If n is 3, clear entire screen and delete all lines saved in the scrollback buffer.
 */
export const ed = (n = 0) => `${CSI}${n}J`;

/** Erases part of the line based on n.
 * If n is 0 (or missing), clear from cursor to the end of the line.
 * If n is 1, clear from cursor to beginning of the line.
 * If n is 2, clear entire line.
 * Cursor position does not change.
 */
export const el = (n = 0) => `${CSI}${n}K`;

/** Sets colors and style of the characters following this code */
export const sgr = (n = 0, ...args: number[]) => `${CSI}${[n, ...args].join(';')}m`;

/** Set foreground color to rgb */
export const rgb = (r: number, g: number, b: number) => sgr(38, 2, r, g, b);
/** Set background color to rgb */
export const rgbBg = (r: number, g: number, b: number) => sgr(48, 2, r, g, b);

/** Format terminal
 * Return formatted string from all given args, join without spaces.
 * Also, replaces all recognized `$words` with matching ant of `sgr.$prop`.
 */
export const fmtt = (...args: unknown[]) =>
  args.map(arg => {
    if (typeof arg === "string") {
      // deno-lint-ignore no-explicit-any
      return arg.replace(/\$(\w+)/g, (match) => (sgr as any)[match] || match)
    } else {
      return arg?.toString() ?? '' + arg;
    }
  }).join('') + sgr.$reset;

sgr.$reset = sgr(0);
sgr.$resetFg = sgr(39);
sgr.$resetBg = sgr(49);

sgr.$black = sgr(30);
sgr.$red = sgr(31);
sgr.$green = sgr(32);
sgr.$yellow = sgr(33);
sgr.$blue = sgr(34);
sgr.$magenta = sgr(35);
sgr.$cyan = sgr(36);
sgr.$white = sgr(37);
sgr.$blackHL = sgr(90);
sgr.$redHL = sgr(91);
sgr.$greenHL = sgr(92);
sgr.$yellowHL = sgr(93);
sgr.$blueHL = sgr(94);
sgr.$magentaHL = sgr(95);
sgr.$cyanHL = sgr(96);
sgr.$whiteHL = sgr(97);
sgr.$bgBlack = sgr(40);
sgr.$bgRed = sgr(41);
sgr.$bgGreen = sgr(42);
sgr.$bgYellow = sgr(43);
sgr.$bgBlue = sgr(44);
sgr.$bgMagenta = sgr(45);
sgr.$bgCyan = sgr(46);
sgr.$bgWhite = sgr(47);
sgr.$bgBlackHL = sgr(100);
sgr.$bgRedHL = sgr(101);
sgr.$bgGreenHL = sgr(102);
sgr.$bgYellowHL = sgr(103);
sgr.$bgBlueHL = sgr(104);
sgr.$bgMagentaHL = sgr(105);
sgr.$bgCyanHL = sgr(106);
sgr.$bgWhiteHL = sgr(107);
