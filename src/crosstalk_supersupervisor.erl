-module(crosstalk_supersupervisor).
-compile(export_all).

%%% Top-level supervisor - in other words, starts everything up.

behavior_info(callbacks) ->
    [{init, 1}].

start_link(Port) ->
    supervisor:start_link(?MODULE, Port).

init(Port) ->
    {ok, {{one_for_one, 0, 1},
          [{channel_supervisor,
           {channel_supervisor, start_link, []},
            permanent,
            2000,
            supervisor,
            [channel_supervisor, channel]},
           {channel_registry,
            {channel_registry, start_link, []},
            permanent,
            2000,
            worker,
            [channel_registry, channel_supervisor, channel]},
           {client_session_initiator,
            {client_session_initiator, start_link, [Port]},
            permanent,
            2000,
            worker,
            [client_session_initiator, client_proxy]}]}}.

