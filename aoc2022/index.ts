
import { existsSync, readFileSync } from "fs";

export * from './src/day01';
export * from './src/day02';
export * from './src/day03';
export * from './src/day04';
export * from './src/day05';
export * from './src/day06';
export * from './src/day07';
export * from './src/day08';
export * from './src/day09';
export * from './src/day10';
export * from './src/day11';
export * from './src/day12';
export * from './src/day13';
export * from './src/day14';
// export * from './src/day15';


/**
 * RUNNER
 */
const cmd = process.argv.pop() || '';
const toRun = module.exports[cmd];
if (toRun) {
    if (existsSync(`./data/${cmd}.txt`)) {
        toRun(readFileSync(`./data/${cmd}.txt`, 'utf8'));
    } else {
        toRun();
    }
}
else {
    console.log("Unknown command: " + cmd);
}
