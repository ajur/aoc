

declare global {
    interface Array<T> {
        sum(): number;
        inGroupOf(n: number): Array<Array<T>>;
        intersection(a: Array<T>): Array<T>;
        unique(): Array<T>;
        last(): T;
        sortAsc(this: Array<number>): Array<number>;
        sortDesc(this: Array<number>): Array<number>;
    }
    interface String {
        inGroupOf(n: number): String[];
        toInt(): number;
    }
}


Array.prototype.sum = function (this: number[]): number {
    let sum = 0;
    for (let i = 0; i < this.length; i++) {
        sum += this[i];
    }
    return sum;
}
Array.prototype.inGroupOf = function <T>(this: T[], n: number): T[][] {
    let arr = [];
    for (let i = 0; i < this.length; i += n) {
        arr.push(this.slice(i, i + n));
    }
    return arr;
}
Array.prototype.intersection = function <T>(this: T[], arr: T[]): T[] {
    return this.filter(x => arr.indexOf(x) !== -1);
}
Array.prototype.unique = function <T>(this: T[]): T[] {
    return this.filter((x, i) => this.indexOf(x) === i);
}
Array.prototype.last = function <T>(this: T[]): T {
    return this[this.length - 1];
}
Array.prototype.sortAsc = function (this: number[]): number[] {
    return this.sort((a, b) => a - b);
}
Array.prototype.sortDesc = function (this: number[]): number[] {
    return this.sort((a, b) => b - a);
}


String.prototype.inGroupOf = function (this: string, n: number): string[] {
    let arr = [];
    for (let i = 0; i < this.length; i += n) {
        arr.push(this.slice(i, i + n));
    }
    return arr;
}
String.prototype.toInt = function (this: string): number {
    return parseInt(this, 10);
}

export {};

