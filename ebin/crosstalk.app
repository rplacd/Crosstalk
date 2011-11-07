{application, crosstalk,
 [{mod, {crosstalk, []}},
  {vsn, "0"},
  {modules, [crosstalk, crosstalk_supersupervisor, channel_supervisor, client_session_initiator, channel_registry, client_proxy, channel]},
  {registered, [channel_supervisor, channel_registry]},
  {env, [{port, 10050}]}]}.
