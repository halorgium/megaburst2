{application, megaburst2,
 [{description, "Megaburst 2"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl, inets]},
  {mod, {megaburst2, []}},
  {modules, [megaburst2, master, herder, leech_sup, leech, peer, tracker, metainfo, bencoding]},
  {registered, [master, herder, peer_sup]}
 ]}.
