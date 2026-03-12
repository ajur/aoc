import { cpl, ed } from "./ansi.ts";
import { isJupyter, isVerbose } from "./common.ts";

let _lastDisplayId: string = '';
let _lastMsgLen: number = 0;
const _countLines = (s: string) => {
  let nls = 1;
  for (let i = 0; i < s.length; ++i) {
    if (s.charAt(i) === '\n') ++nls;
  }
  return nls;
}
export const display = async (target: unknown, did?: string) => {
  if (!did) {
    did = _lastDisplayId = `ulogDid-${Math.floor(Math.random() * 100000)}`;
  }
  if (isJupyter && typeof target === "string") {
    await Deno.jupyter.broadcast("display_data", {
      data: { "text/plain": target },
      metadata: {},
      transient: { display_id: did },
    });
  } else if (isJupyter) {
    await Deno.jupyter.display(target ?? '', {display_id: did});
  } else if (isVerbose) {
    const trg = typeof target === "string" ? target : (target?.toString() ?? (''+target));
    console.log(trg);
    _lastMsgLen = _countLines(trg);
  }
}

export const updateDisplay = async (target: unknown, did?: string) => {
  did ??= _lastDisplayId;
  if (isJupyter && typeof target === "string") {
    await Deno.jupyter.broadcast("update_display_data", {
      data: { "text/plain": target },
      metadata: {},
      transient: { display_id: did },
    });
  } else if (isJupyter) {
    await Deno.jupyter.display(target, { display_id: did, update: true});
  } else if (isVerbose) {
    const trg = typeof target === "string" ? target : (target?.toString() ?? (''+target));
    console.log(cpl(_lastMsgLen) + ed() + trg);
    _lastMsgLen = _countLines(trg);
  }
}
