import {readFileSync, existsSync} from "fs";

const setup = {
  day: 0,
  useSample: false,
  data: null,
  debug: false,
  a: false,
  b: false
}

const args = [...process.argv]
while (args.length > 2) {
  const arg = args.pop().toLowerCase();
  if (arg.match(/^(s|sample)$/)) {
    setup.useSample = true;
  }
  else if (arg.match(/^(d|debug)$/)) {
    setup.debug = true;
  }
  else if (arg.match(/^(\d+)$/)) {
    setup.day = parseInt(arg, 10);
    setup.dayStr = arg.padStart(2, '0')
  }
  else if (arg === 'a') {
    setup.a = true;
  }
  else if (arg === 'b') {
    setup.b = true;
  }
}

if (!setup.a && !setup.b) {
  setup.a = setup.b = true;
}

const srcFile = `./src/day${setup.dayStr}.js`;
const dataFile = `./data/day${setup.dayStr}.txt`;

if (!existsSync(srcFile)) {
  console.log(`No sources for day ${setup.day} yet :(`)
  process.exit()
}

if (!setup.useSample && !existsSync(dataFile)) {
  console.log(`No data for day ${setup.day} yet :(`)
  process.exit()
}

const rawData = setup.useSample ? null : readFileSync(dataFile, 'utf-8');

const module = await import(srcFile);
 
if (setup.a && module.runA) {
  const dataA = (rawData || module.sampleA || module.sample || '').trim();
  console.log(`Day ${setup.day}:A:`, module.runA(dataA));
}

if (setup.b && module.runB) {
  const dataB = (rawData || module.sampleB || module.sample || '').trim();
  console.log(`Day ${setup.day}:B:`, module.runB(dataB));
}

