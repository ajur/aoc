
import {grid as G, strings as S, arrays as A} from './utils.js'

export const sample = `
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
`

export const runA = (data) => {
  const {workflows, parts} = parse(data, parseRuleAsFunc);
  const processor = wfProcessor(workflows);

  return parts.filter(processor)
    .map(Object.values)
    .flat()
    .sum()
}

export const runB = (data) => {
  const {workflows} = parse(data, parseRuleAsRanges);
  
  const accepted = [];
  const rejected = [];
  const toProcess = [
    ['in', {
      x: [1, 4000],
      m: [1, 4000],
      a: [1, 4000],
      s: [1, 4000]
    }]];

  while (toProcess.length > 0) {
    let [nextWf, ranges] = toProcess.shift();
    if (nextWf === 'A') {
      accepted.push(ranges);
    } else if (nextWf === 'R') {
      rejected.push(ranges);
    } else {
      for (const rule of workflows[nextWf]) {
        const {wf, pass, fail} = rule(ranges);
        if (pass) {
          toProcess.push([wf, pass])
        }
        if (fail) {
          ranges = fail;
        } else {
          break;
        }
      }
    }  
  }

  return accepted.map(v => Object.values(v)
    .map(([a,b]) => b - a + 1)
    .reduce((a,v) => a*v)
  ).sum()
}

const wfProcessor = workflows => part => {
  let wf = 'in';
  while(wf != 'A' && wf != 'R') {
    wf = processWorkflow(workflows[wf], part);
  }
  return wf === 'A';
}

const processWorkflow = (wf, part) => {
  for (const rule of wf) {
    const out = rule(part);
    if (out) {
      return out;
    }
  }
}

const parse = (str, ruleParser) => str.split('\n\n')
  .map(S.split('\n'))
  .apply(([wfs, ps]) => ({
    workflows: parseWorkflows(wfs, ruleParser),
    parts: ps.map(pl => eval('('+pl.replaceAll('=',':')+')'))
  }))

const parseWorkflows = (wfsl, ruleParser) => wfsl
  .map(wfs => wfs.match(/^(\w+)\{(.+)\}/))
  .map(([_, key, rules]) => [key, rules.split(',').map(ruleParser)])
  .apply(Object.fromEntries)

const parseRuleAsFunc = (rule) => {
  const match = rule.match(/([xmas])([<>])(\d+):(\w+)/)
  if (match) {
    const [_, k, op, vs, wf] = match;
    const v = parseInt(vs);
    if (op === '>')
      return (o) => o[k] > v ? wf : null;
    else
      return (o) => o[k] < v ? wf : null;
  }
  return (o) => rule;
}

const parseRuleAsRanges = (rule) => {
  const match = rule.match(/([xmas])([<>])(\d+):(\w+)/)
  if (match) {
    const [_, k, op, vs, wf] = match;
    const v = parseInt(vs);
    if (op === '>')
      return rangeRuleGT(k, v, wf);
    else
      return rangeRuleLT(k, v, wf);
  }
  return rangeRulePass(rule);
}

const rangeRuleGT = (key, val, wf) => (ranges) => {
  const [low, high] = ranges[key];
  if (low > val)
    return {wf, pass: ranges}
  if (high <= val)
    return {fail: ranges}
  return {wf,
    fail: {...ranges, [key]: [low, val]},
    pass: {...ranges, [key]: [val + 1, high]}
  }
};
const rangeRuleLT = (key, val, wf) => (ranges) => {
  const [low, high] = ranges[key];
  if (high < val)
    return {wf, pass: ranges}
  if (low >= val)
    return {fail: ranges}
  return {wf,
    pass: {...ranges, [key]: [low, val - 1]},
    fail: {...ranges, [key]: [val, high]}
  }
};
const rangeRulePass = (wf) => (ranges) => ({wf, pass: ranges});
