
import {functions as F, maths as M} from './utils.js'

const sample1 = `
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
`
const sample2 = `
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
`

export const sample = sample2

export const runA = (data) => parse(data)
  .apply(modules => {
    const N = 1000;
    const pushCount = [0, 0];
    for (let i = 0; i < N; ++i) {
      const out = pushTheButton(modules);
      pushCount[0] += out[0];
      pushCount[1] += out[1];
    }

    return pushCount[0] * pushCount[1];
  });


export const runB = (data) => parse(data)
  .apply(modules => {
    const preRx = Object.values(modules).filter(m => m.targets.indexOf('rx') >= 0);
    if (preRx.length !== 1) {
      return "RX HAS MORE SOURCES";
    }
    const preRxName = preRx[0]._name;
    const preRxSources = preRx[0].sources.reduce((m, n) => Object.assign(m, {[n]: 0}), {});
      
    let i = 0;
    while (true) {
      ++i;
      const prePreRx = pushTheButton(modules, preRxName)[2];
      if (prePreRx && !preRxSources[prePreRx]){
        console.log(prePreRx, preRxSources)
        preRxSources[prePreRx] = i;
        const mult = Object.values(preRxSources).reduce((agg, v) => agg * v, 1);
        if (mult > 0) {
          return M.lcm(...Object.values(preRxSources));
        }
      };
    }
  });


const pushTheButton = (modules, preRx) => {
  const queue = [{from: 'button', to: 'broadcaster', value: 0}];
  const pulseCount = [0, 0];
  let preRxFrom = null;
  while (queue.length > 0) {
    const pulse = queue.shift();
    pulseCount[pulse.value] += 1;
    if (preRx && pulse.to === preRx && pulse.value === 1) {
      preRxFrom = pulse.from;
    }
    
    const module = modules[pulse.to];
    if (module) queue.push(...(module(pulse)));
  }
  return [...pulseCount, preRxFrom];
}

const broadcaster = ({name, targets}) => ({value}) => targets.map(t => ({from: name, to: t, value}));
const flipFlop = ({name, targets, state = 0}) => ({value}) => {
  if (!value) {
    state = (state + 1) % 2;
    return targets.map(t => ({from: name, to: t, value: state}));
  }
  return [];
};
const conjunction = ({name, targets, sources, state = sources.map(v => 0)}) => ({from, value}) => {
  state[sources.indexOf(from)] = value;
  const out = state.every(v => v === 1) ? 0 : 1;
  return targets.map(t => ({from: name, to: t, value: out}));
}

const specToModule = (spec) => {
  let module = null;
  switch (spec.type) {
    case 'broadcaster': module = broadcaster(spec); break;
    case '%': module = flipFlop(spec); break;
    case '&': module =  conjunction(spec); break;
  }
  module._name = spec.name;
  module.targets = spec.targets;
  module.sources = spec.sources;
  return module;
}


const parse = (str) => str.split('\n')
  .map(l => l.split(' -> '))
  .map(([name, out]) => ({
    type: name[0] === '&' || name[0] === '%' ? name[0] : name === 'broadcaster' ? name : null,
    name: name[0] === '&' || name[0] === '%' ? name.substring(1) : name,
    targets: out.split(', ')
  }))
  .apply(modulesSpecs => {
    const targetToSourcesMap = modulesSpecs.reduce((t2s, {name, targets}) => {
      for (const target of targets) {
        t2s[target] ??= [];
        t2s[target].push(name);
      }
      return t2s;
    }, {});
    modulesSpecs.forEach(spec => {
      spec.sources = targetToSourcesMap[spec.name] || []
    });

    return modulesSpecs;
  })
  // .peek(printGraphviz)
  .reduce((modules, spec) => Object.assign(modules, {[spec.name]: specToModule(spec)}), {})


const printGraphviz = (modulesSpecs) => {
  const colorMap = {'broadcaster': 'greenyellow', '%': 'lightskyblue', '&': 'gold', 'out': 'orangered'}
  const shapeMap = {'broadcaster': 'ellipse', '%': 'hexagon', '&': 'invhouse', 'out': 'Msquare'}
  const nodes = modulesSpecs.map(F.key('name'));
  const leafTargets = modulesSpecs.flatMap(F.key('targets')).unique().filter(v => nodes.indexOf(v) < 0).map(name => ({name, type: 'out'}))
  const edges = modulesSpecs.map(({name, targets}) => `${name} -> {${targets.join(' ')}}`)
  const types = [...modulesSpecs, ...leafTargets].map(({name, type}) => `${name} [style="filled", fillcolor="${colorMap[type]}", shape="${shapeMap[type]}"]`)
  console.log('output for Graphviz, eg: https://dreampuf.github.io/GraphvizOnline/')
  console.log('digraph G {\n\n' + edges.join('\n') + '\n\n' + types.join('\n') + '\n}\n')
}

