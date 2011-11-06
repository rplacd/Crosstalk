-module(client_proxy).
-compile(export_all).

%%% The Client Proxy - where input from a single client (wrapped by a message forwarder) is parsed and data is sent to that client, 
%%% and as a result where all the client logic occurs: 
%%% joining channels, posting messages, recieving messages, nicknames.


%%% Two states exist: uninitialized, and initialized. We start with the former.
%%% Client will handshake - send "hello $NICKNAME", server will send "hi $NICKNAME" back, and then everything else begins.

%%% Messages are lists delimited by a particular character (look at seperator_char/0).
%%% Async calls - "I'm", "leave", and "post" return nothing,
%%% While synchronous calls - ""join" - either send a ["success", Message] or a ["failure", Reason, Message].


-record(state,
        {nick,
         name_to_channel = dict:new(),
         message_forwarder}).

behavior_info(callbacks) ->
    [{init, 1}, {handle_cast, 2}, {handle_call, 3}, {handle_info, 3}, {terminate, 2}].

%%% Wrapper functions over gen_server calls.
start(MessageForwarder) ->
%assumption: MessageForwarder's going to link to us later on.
    gen_fsm:start(?MODULE, [MessageForwarder], []).

client_nick(Client) ->
    gen_fsm:sync_send_event(Client, client_nick).


% Not a wrapper, but here because from_client must have its partner that sends data to its MessageForwarder.
% Both of these functions take and return lists of strings - it handles the automagic formatting to and from the data we actually send down the pipes.
% Both of these functions also currently wrap functions that strip and add the surreptitious telnet CRLF.
to_message_forwarder(MessageForwarder, Message) ->
    Sending = format_telnet(structure_message(Message)),
    io:format("Sending ~s~n", [Sending]),
    MessageForwarder ! {to_client, format_telnet(structure_message(Message))}.

to_client(Client, Message) ->
    gen_fsm:send_event(Client, {to_client, Message}).

from_client(Client, Message) ->
    gen_fsm:send_event(Client, {from_client, destructure_message(clean_telnet(Message))}).

%%% gen_server callbacks.
init([MessageForwarder]) ->
    monitor(process, MessageForwarder),
    {ok, uninitialized, #state{message_forwarder = MessageForwarder}}.

% The handshaking portion.
uninitialized({from_client, Message}, State=#state{message_forwarder=MessageForwarder}) ->
    case Message of
        ["I'm", Nick] ->
            to_message_forwarder(MessageForwarder, ["hi", Nick]),
            {next_state, initialized, State#state{nick=Nick}};
        _ ->
            to_message_forwarder(MessageForwarder, ["what"]),
            {next_state, uninitialized, State}
    end.
    
% And now the real beef of the protocol - once we've handshaked...

% Hand off the actual message processing to another function so we can patternmatch in the argument list.
initialized({from_client, Message}, State=#state{}) ->
    {next_state, initialized, handle_message(Message, State)};
% Currently hardcoding in the case of to_client that Channel uses.
% We might eventually do what from_client does above and further dispatch.
initialized({to_client, {ChannelPid, {_SrcPid, SrcNick}, Message}}, 
            State = #state{name_to_channel=ChannelMappings, message_forwarder=MessageForwarder}) ->
    ChannelName = dict:fold(fun(CurrName, CurrPid, AccIn) ->
                                case {CurrPid, AccIn}  of
                                    {ChannelPid, _} -> CurrName;
                                    {_, _}  -> AccIn
                                end
                            end, no_channel_found, ChannelMappings),
    case ChannelName of
        no_channel_found ->
            {next_state, initialized, State};
        String when is_list(String) ->
            to_message_forwarder(MessageForwarder, ["message", ChannelName, SrcNick, Message]),
            {next_state, initialized, State}
    end.
initialized(client_nick, {_Pid, _MsgTag}, State=#state{nick = Nick}) ->
    {reply, Nick, initialized, State}.

% When the message forwarder crashes, we terminate...
handle_info({'DOWN', _ref, process, MessageForwarder, _Reason}, StateName, #state{message_forwarder=MessageForwarder}) ->
    exit(normal).

% When we terminate - either we crash, or the client disconnects and the client_proxy sends its death to us, remove ourselves from the channels we've connected to (that being the only real dependency we have to look after).
terminate(_Reason, _StateName, _State=#state{nick=Nick, name_to_channel=ChannelMap}) ->
    dict:map(fun(_ChanName, ChanPid) ->
                     channel:remove_client(ChanPid, self(), Nick)
             end, ChannelMap).
                     

%%% Message handlers - sent from a from_client message. See above...
%%% Must return an updated state record. Just the state record, though, since we don't expect to ever leave "initialized".
handle_message(OriginalMsg=["join", ChannelName], State=#state{nick=Nick, name_to_channel=ChannelMappings, message_forwarder=MessageForwarder}) ->
    case dict:is_key(ChannelName, ChannelMappings) of
        true -> 
            to_message_forwarder(MessageForwarder, make_failure_msg("already_joined", OriginalMsg)),
            state;
        false ->
            % look at channel_registry to see why we can just join willy-nilly - it handles the case where the channel doesn't exist as well...
            Channel = channel_registry:get_channel(channel_registry, ChannelName),
            case channel:add_client(Channel, self(), Nick) of
                nick_already_exists ->
                    to_message_forwarder(MessageForwarder, make_failure_msg("nick_already_exists", OriginalMsg)),
                    State;
                success ->
                    to_message_forwarder(MessageForwarder, make_success_msg(OriginalMsg)),
                    State#state{name_to_channel=dict:store(ChannelName, Channel, ChannelMappings)}
            end
    end;
handle_message(["leave", ChannelName], State=#state{nick=Nick, name_to_channel=ChannelMappings}) ->
    case dict:find(ChannelName, ChannelMappings) of
        error -> State;
        {ok, ChannelPid} -> 
            channel:remove_client(ChannelPid, self(), Nick),
            State
    end;
handle_message(["post", ChannelName | RawMessage], State=#state{nick=Nick, name_to_channel=ChannelMappings}) ->
    % first, though, we have to recover the original message from the list that is RawMessage. Remember that structure_message and destructure_message are inverse functions.
    Message = structure_message(RawMessage),
    case dict:find(ChannelName, ChannelMappings) of
        error ->
            State;
        {ok, ChannelPid} ->
            channel:post_message(ChannelPid, self(), Nick, Message),
            State
    end;
handle_message(NoUnderstando, State=#state{}) ->
    io:format("I don't understand the message ~w~n", [NoUnderstando]),
    State.

%%% Utility functions for parsing and creating messages - basically seperator-split lists of strings.

% Make string lists for successful sync calls and failed calls.
make_success_msg(OriginalMsg) ->
    ["success", OriginalMsg].
make_failure_msg(Reason, OriginalMsg) ->
    ["failure", Reason, OriginalMsg].

% Clean the CRLF off telnet messages.
clean_telnet(Raw) ->
    case string:tokens(Raw, "\r\n") of
        [Nope|[]] -> Nope;
        [Filtered|_Anything_Else_Well_Hard_Luck] -> Filtered;
        [] -> []
    end.
% Add a CRLF to telnet messages.
format_telnet(Raw) ->
    lists:append(Raw, "\r\n").

% Get the seperator character.
seperator() ->
    $\s.

% A utility function that destructures messages - "hey\nfloyd" -> ["hey", "floyd"].
% Preserves inbetween unit seperators - thus the length of the output list is always tokens + 1.
destructure_message(String) ->
    parse_internal(String, seperator()).
parse_internal(Src, SepChar) when is_integer(SepChar) ->
    {ok, SplitList} = regexp:split(Src, [SepChar]),
    SplitList.

% Create a formatted message - bodge the strings together and intercalate (thanks, Haskell!) $\ns inbetween.
structure_message(StringOfStrings) ->
    lists:flatten(intersperse(seperator(), StringOfStrings)).

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X|Xs]) -> [X|[Sep|intersperse(Sep, Xs)]].
    


%%% Testing.
test() ->
    true = destructure_message("") == [""],
    true = destructure_message("ornithologist") == ["ornithologist"],
    true = destructure_message("orni\nthologist") == ["orni", "thologist"],
    true = destructure_message("\nornithologist") == ["", "ornithologist"],
    true = destructure_message("ornithologist\n") == ["ornithologist", ""],
    true = destructure_message("\n") == ["",""],
    true = destructure_message("\n\n") == ["", "", ""],
    true = destructure_message("\nornithologist\n") == ["", "ornithologist", ""],
    true = structure_message([""]) == "",
    true = structure_message([]) == "",
    true = structure_message(["telstar"]) == "telstar".
    %true = clean_telnet("derp") == "derp",
    %true = clean_telnet("\r\n") == "\r\n",
    %true = clean_telnet("derp\r\n") == "derp",
    %true = format_telnet("") == "\r\n",
    %true = format_telnet("derp") == "derp\r\n".
