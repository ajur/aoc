import { assertEquals } from "@std/assert";
// %%

type FromArg = string
  | [number, number]
  | { min: number, max: number, minClosed?: boolean, maxClosed?: boolean };

export class Interval {
  readonly min: number;
  readonly max: number;
  readonly minClosed: boolean;
  readonly maxClosed: boolean;

  constructor(min = 0, max = 0, minClosed = true, maxClosed = false) {
    if (min > max) {
      this.min = this.max = 0;
      this.minClosed = this.maxClosed = false;
    } else if (min === max) {
      this.min = this.max = min;
      this.minClosed = this.maxClosed = minClosed || maxClosed;
    } else {
      this.min = min;
      this.max = max;
      this.minClosed = minClosed;
      this.maxClosed = maxClosed;
    }
  }

  static from(ob: FromArg | FromArg[] | Interval[]): Interval {
    if (typeof ob === 'object' && 'min' in ob && 'max' in ob) {
      return new Interval(ob.min, ob.max, ob.minClosed, ob.maxClosed);
    }
    if (typeof ob === 'string') {
      if (ob.match(/^[\d\.]+-[\d\.]+$/)) {
        const nums = ob.split('-');
        return new Interval(+nums[0], +nums[1], true, true);
      }
      const rangeMatch = ob.match(/^([\(\[])(-?[\d\.]+)[^\d\.-]+(-?[\d\.]+)([\)\]])$/);
      if (rangeMatch) {
        return new Interval(
          +rangeMatch[2],
          +rangeMatch[3],
          rangeMatch[1] === '[',
          rangeMatch[4] === ']'
        );
      }
      throw new Error(`Unsupported interval string format: ${ob}`);
    }
    if (Array.isArray(ob) && typeof ob[0] === "number") {
      if (ob.length !== 2 || typeof ob[1] !== "number") {
        throw new Error(`Unsupported interval array format: ${ob}`);
      }
      return new Interval(ob[0], ob[1]);
    }
    if (Array.isArray(ob)) {
      const intervals = [];
      for (const o of ob) {
        try {
          intervals.push(o instanceof Interval ? o : Interval.from(o as FromArg));
        } catch(e) {
          console.error("Skipping faulty interval spec", o, "cause of error:", e);
        }
      }
      return combineIntervals(...intervals);
    }
    throw new Error(`Unsupported Interval.from parameter format: ${ob}`);
  }

  get isEmpty(): boolean {
    if (this.min < this.max) return false;
    if (this.min > this.max) return true;
    // min === max
    if (this.minClosed && this.maxClosed) return false;
    return true;
  }

  eq(r: Interval): boolean {
    return this.min === r.min && this.max === r.max && this.minClosed === r.minClosed && this.maxClosed === r.maxClosed;
  };

  toString(): string {
    return this.isEmpty ? '(∅)' : `${this.minClosed ? '[' : '('}${this.min},${this.max}${this.maxClosed ? ']' : ')'}`;
  }

  includes(num: number): boolean {
    return (this.minClosed ? num >= this.min : num > this.min)
      && (this.maxClosed ? num <= this.max : num < this.max);
  }

  combine(...intervals: Interval[]): Interval {
    return combineIntervals(this, ...intervals);
  }

  countInts(): number {
    if (this.isEmpty) return 0;

    let min = Math.ceil(this.min);
    if (min === this.min && !this.minClosed) {
      min += 1;
    }
    let max = Math.floor(this.max);
    if (max === this.max && !this.maxClosed) {
      max -= 1;
    }

    return max - min + 1;
  }
}

export class MultiInterval implements Interval {
  readonly ranges: Interval[];

  constructor(intervals?: Interval[]) {
    this.ranges = simplifyIntervals(intervals || []);
  }

  get isEmpty(): boolean {
    return this.ranges.length === 0;
  }

  get min(): number {
    return this.isEmpty ? Infinity : this.ranges[0].min;
  };
  get max(): number {
    return this.isEmpty ? -Infinity : this.ranges.at(-1)!.max;
  };
  get minClosed(): boolean {
    return this.isEmpty ? false : this.ranges[0].minClosed;
  };
  get maxClosed(): boolean {
    return this.isEmpty ? false : this.ranges.at(-1)!.maxClosed;
  };

  eq(r: MultiInterval | Interval): boolean {
    if (r instanceof MultiInterval) {
      if (this.ranges.length !== r.ranges.length) return false;
      for (let i = 0; i < this.ranges.length; ++i) {
        if (!this.ranges[i].eq(r.ranges[i])) return false;
      }
      return true;
    }
    return this.ranges.length === 1 && this.ranges[0].eq(r);
  }

  toString(): string {
    return this.ranges.map(r => r.toString()).join('∪');
  }

  includes(num: number): boolean {
    if ((this.minClosed ? num >= this.min : num > this.min) && (this.maxClosed ? num <= this.max : num < this.max)) {
      for (const range of this.ranges) {
        if (range.includes(num)) return true;
      }
    }
    return false;
  }

  combine(...intervals: Interval[]): Interval {
    return combineIntervals(...this.ranges, ...intervals);
  }

  countInts(): number {
    return this.ranges.map(r => r.countInts()).reduce((s, v) => s + v, 0);
  }
}

function simplifyIntervals(intervals: Interval[]): Interval[] {
  const ranges = intervals
    .flatMap(i => i instanceof MultiInterval ? i.ranges : i)
    .filter(invl => !invl.isEmpty);

  if (ranges.length === 0) return [];
  if (ranges.length === 1) return [ranges[0]];

  ranges.sort((r1, r2) => {
    if (r2.min !== r1.min) return r2.min - r1.min;
    if (r1.minClosed) return 1;
    if (r2.minClosed) return -1;
    return 0;
  });

  const out: Interval[] = [ranges.pop()!];

  while(ranges.length > 0) {
    const prev = out.pop()!
    const next = ranges.pop()!;
    // console.log("-- prev", prev.toString(), "next", next.toString());
    if (prev.includes(next.min) && prev.includes(next.max)) {
      out.push(prev);
      // console.log("-- - next fully enclosed, push", prev.toString())
      continue;
    }
    if (next.min > prev.max || (next.min === prev.max && !next.minClosed && !prev.maxClosed)) {
      out.push(prev);
      out.push(next);
      // console.log("-- - next fully after prev, push", prev.toString(), "then", next.toString())
      continue;
    }
    if (next.min < prev.max || (next.min === prev.max && (next.minClosed || prev.maxClosed))) {
      out.push(new Interval(
        prev.min, next.max,
        prev.minClosed, next.maxClosed
      ));
      // console.log("-- - next start overlaps or touches prev end, combine and push", out.at(-1)!.toString())
      continue;
    }
    throw new Error(`Unexpected merging case with intervals: prev=${prev.toString()} next=${next.toString()}`)
  }
  return out
}

function combineIntervals(...intervals: Interval[]): Interval {
  const out = simplifyIntervals(intervals);
  return out.length === 0
    ? new Interval()
    : out.length === 1
      ? out[0]
      : new MultiInterval(out);
}
// %%

assertEquals(Interval.from("1-4").toString(), "[1,4]")
assertEquals(Interval.from("4-1").toString(), "(∅)")
assertEquals(Interval.from("[-4; -1]").toString(), "[-4,-1]")
assertEquals(Interval.from("[5:8)"), new Interval(5, 8, true, false));
assertEquals(new MultiInterval([Interval.from("[1, 4]"), Interval.from("[5:8)")]).toString(), "[1,4]∪[5,8)")

assertEquals(
  combineIntervals(...["1-3","2-0", "(1.5,2.5)", "[3.1, 4)", "(3.9, 5)", "[5, 6]", "(6, 7)", "(7, 8)"].map(Interval.from)).toString(),
  "[1,3]∪[3.1,7)∪(7,8)");
assertEquals(
  combineIntervals(...["(-5,-3)","[-4,2]", new Interval(2, 3)].map(Interval.from)).toString(),
  "(-5,3)");
assertEquals(
  combineIntervals(Interval.from("1-3,4-5,2-4".split(','))).toString(),
  "[1,5]");
assertEquals(
  Interval.from("1-5")
    .combine(Interval.from("(8,10)"))
    .combine(Interval.from("3-9")).toString(),
  "[1,10)"
)

assertEquals(Interval.from("[3,7]").countInts(), 5);
assertEquals(Interval.from(["[3, 7]", "(-7,-3)"]).countInts(), 8);
