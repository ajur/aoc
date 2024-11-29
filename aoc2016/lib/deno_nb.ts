export const ulog = async (did: string, msg?: string) => {
  await Deno.jupyter.broadcast("display_data", {
    data: { "text/plain": msg ?? '' },
    metadata: {},
    transient: { display_id: did },
  });

  return async (msg: string) => {
    await Deno.jupyter.broadcast("update_display_data", {
      data: { "text/plain": msg },
      metadata: {},
      transient: { display_id: did },
    });
  }
}
