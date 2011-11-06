-module(channel_registry).
-compile(export_all).

%%% The Channel Registry - a singleton process that gets Channel pids mapped to their names - if one doesn't exist, create it.

% Seems too centralized - if this turns out to be the bottleneck (so keep your channels once you've got them, or let them contact you!), 
% opt to run multiple channel registries off a single db backend.

-record(state, 
        {name_to_channels = dict:new(),
        name_to_monitor = dict:new()}).
% name_to_channels is the place you want to go to to see if a channel exists.

behaviour_info(callbacks) ->
    [{init, 1}, {handle_cast, 2}, {handle_call, 3}, {terminate, 2}].

%%% Wrapper functions over gen_server calls.
start() ->
    gen_server:start(?MODULE, [], []).
% Get a Channel pid for Name - if it doesn't exist, create one.
get_channel(Registry, Name) ->
    gen_server:call(Registry, {get_channel, Name}).

%%% gen_server callbacks.
init([]) ->
    % clobber any current server registered to the same atom
    catch unregister(?MODULE),
    register(?MODULE, self()),
    {ok, #state{}}.

handle_call({get_channel, Name}, {_ClientPid, _MsgTag}, State=#state{name_to_channels = Channels, name_to_monitor = Monitors}) ->
    try
        {reply, dict:fetch(Name, Channels), State}
    catch
        error:badarg ->
            {_, Channel} = channel:start(),
            {reply, Channel,
             State#state{name_to_channels=dict:store(Name, Channel, Channels),
                         name_to_monitor=dict:store(Name, monitor(process, Channel) ,Monitors)}}
    end.

terminate(_Reason, _State) ->
% provisional: when we terminate, do jack-squat.
    nonce.

%%% Testing.
test() ->
    {_, Registry} = start(),
    Channel = get_channel(Registry, "wallyworld"),
    Channel = get_channel(Registry, "wallyworld").
