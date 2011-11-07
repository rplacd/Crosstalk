-module(channel).
-compile([export_all]).

%%% Channel - a glorified dispatcher where multiple clients can add themselves to be notified of messages posted, or post messages themselves.

-record(state,
        {pid_to_nick = dict:new(),
         pid_to_monitor = dict:new()}).  
% why not just tuple nick and monitor together?
% because, lad. anyway, pid_to_nick is authoritative - so check for membership there [first].

behaviour_info(callbacks) ->
    [{init, 1}, {handle_cast, 2}, {handle_call, 3}, {handle_info, 2}, {terminate, 2}].

%%% Wrapper functions over gen_server calls. 
start_link() ->
    gen_server:start_link(?MODULE, [], []).
start() ->
    gen_server:start(?MODULE, [], []).
% These are gen_server casts with the exception of add_client, which returns success | nick_already_exists.
post_message(Channel, ClientPid, Nick, Message) ->
    gen_server:cast(Channel, {post_message, {ClientPid, Nick}, Message}).
add_client(Channel, ClientPid, Nick) ->
    gen_server:call(Channel, {add_client, {ClientPid, Nick}}).
remove_client(Channel, ClientPid, Nick) ->
    gen_server:cast(Channel, {remove_client, {ClientPid, Nick}}).

%%% gen_server callbacks.
init([]) ->
    {ok, #state{}}.

% A temporary fudge - silently demonitors a ClientProxy pid and erases it from both the pid-to-nick and pid-to-monitor dicts in state.
% We currently use this when a client disconnects or crashes out.
% Returns a gen_server async value - {noreply, NewState}.
internal_remove_client_async(Pid, State=#state{pid_to_nick=NickMappings, pid_to_monitor=MonitorMappings}) ->
    try
        case {dict:fetch(Pid, NickMappings), dict:fetch(Pid, MonitorMappings)} of
            {_Nick, Monitor} ->
                demonitor(Monitor, [flush]),
                {noreply, State#state{pid_to_nick=dict:erase(Pid, NickMappings),
                                      pid_to_monitor=dict:erase(Pid, MonitorMappings)}}
        end
    catch
        error:badarg -> {noreply, State}
    end.

handle_cast({post_message, PidNickPair, Message}, State=#state{pid_to_nick=NickMappings}) ->
    % we should check whether pidnickpair matches up...
    dict:map(fun(Pid, _Nick) ->
                     client_proxy:to_client(Pid, {self(), PidNickPair, Message})
             end, NickMappings),
    {noreply, State};
handle_cast({remove_client, {Pid, _Nick}}, State=#state{}) ->
    internal_remove_client_async(Pid, State).

handle_call({add_client, {Pid, Nick}}, {_Pid, _Tag}, State=#state{pid_to_nick=NickMappings, pid_to_monitor=MonitorMappings}) ->
    % does the nick already exist?
    case lists:keyfind(Nick, 2, dict:to_list(NickMappings)) of
        {_Pid, _Nick} -> 
            {reply, nick_already_exists, State};
        false ->
            {reply, success, State#state{pid_to_nick=dict:store(Pid, Nick, NickMappings),
                                         pid_to_monitor=dict:store(Pid, monitor(process, Pid), MonitorMappings)}}
    end.

handle_info({'DOWN', _Monitor, _Type, Pid, _CrashInfo}, State=#state{pid_to_nick=NickMappings, pid_to_monitor=MonitorMappings}) ->
    % provisional: if a client crashes, quietly remove it.
    {noreply,
     State#state{pid_to_nick=dict:erase(Pid, NickMappings),
                 pid_to_monitor=dict:erase(Pid, MonitorMappings)}}.

terminate(_Reason, _State) ->
    % provisional: when shutting down, just drop everything.
    noop.
    
%%% Testing.
test() ->
    {_, C} = start(),
    success = add_client(C, self(), "d"),
    success = add_client(C, self(), "hey, you! rocksteady crew."),
    success = add_client(C, spawn(fun()->{}end), "d"),
    nick_already_exists = add_client(C, spawn(fun()->{}end), "hey, you! rocksteady crew.").
    % we can't really test anything else, to be honest - posting messages requires a client_proxy to exist, and removing is - well - removing.
                                                    
                                                     

