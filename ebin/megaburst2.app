{application, megaburst2,
 [{description, "Megaburst 2"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {megaburst2_app, []}},
  {modules, [megaburst2_app, megaburst2_sup]},
  {registered, [megaburst2_sup, megaburst2_server]}
 ]}.
