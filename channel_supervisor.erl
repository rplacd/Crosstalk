-module(channel_supervisor).
-compile(export_all).

%%% The supervisor for channel processes.

behavior_info() ->
    [{init, 1}].

%%% A few wrapper functions...
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
% Spin up a new channel and grab it.
start_child(Supervisor) ->
    supervisor:start_child(Supervisor, []).

%%% Supervisor callbacks.

init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{channel, 
            {channel, start_link, []},
            permanent,
            2000,
            worker,
            [channel, channel_supervisor, client_proxy]}]}}.
