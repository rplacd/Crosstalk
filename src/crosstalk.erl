-module(crosstalk).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
    {ok, Port} = application:get_env(crosstalk, port),
    crosstalk_supersupervisor:start_link(Port).
 
stop(_State) ->
    ok.
