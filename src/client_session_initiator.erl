-module(client_session_initiator).
-compile(export_all).

%%% The Client Session Initiator - a glorified accept loop that, on accept, starts a dumb shim between a client and a Client Proxy.
%%% Where things start, really.

-record(state,
        {listen_socket}).

behavior_info(callbacks) ->
    [{init, 1}, {terminate, 2}].

%%% Wrapper functions over gen_server calls.
start_link(PortNo) ->
    gen_server:start_link(?MODULE, [PortNo], []).
start(PortNo) ->
    gen_server:start(?MODULE, [PortNo], []).

%%% gen_server callbacks.
init([PortNo]) ->
    {ok, ListenSocket} = gen_tcp:listen(PortNo, [list, {packet, 0}, {active, false}, {reuseaddr, true}]),
    spawn_link(make_accept_loop(ListenSocket)),
    {ok, #state{listen_socket=ListenSocket}}.
terminate(_Reason, #state{listen_socket=ListenSocket}) ->
    gen_tcp:close(ListenSocket).
    

%%% The internal workings of the client session initiator - both the singleton accept loop and the shim between socket and ClientProxy which the accept loop both creates and then links.

% Make a function that runs an accept loop and creates child processes.
make_accept_loop(ListenSocket) ->
    fun() ->
            accept_loop(ListenSocket)
    end.
% Works like your standard accept loop - when the accept loop accepts a new connection, it creates both the shim that deals with TCP and the higher-level client proxy.
accept_loop(ListenSocket) ->
%receive with a timeout of 0 here if we want to trap exits.
    case gen_tcp:accept(ListenSocket, 2000) of
        {ok, Socket} ->
            MessageForwarder = spawn(make_message_forwarder_loop(Socket)),
            {ok, ClientProxy} = client_proxy:start(MessageForwarder),
            MessageForwarder ! {set_client_proxy, ClientProxy},
            accept_loop(ListenSocket);            
         % give the loop some time periodically to read its mailbox.
        {error, timeout} ->
            accept_loop(ListenSocket)
    end.

% Make the dumb message forwarder that interfaces with the client_proxy and the client in question.
% Deals with the TCP innards.
make_message_forwarder_loop(Socket) ->                 
    fun() ->
            message_forwarder_loop(Socket, i_started_before_client_proxy)
    end.

% The dumb message forwarder - has no client logic - it takes strings from the sockets and shunts them to the client proxy, and vice versa.
% Exits normally when client disconnects.
message_forwarder_loop(Socket, ClientProxy) ->
% There's a special rub to this message forwarder, though.
% That's the fact that either it or the client_proxy must be created first in order for them to reference each other - and this one gets created first. 
% It just keeps messages in the mailbox until needed...
    case ClientProxy of
        i_started_before_client_proxy ->
            receive
                {set_client_proxy, NewClientProxy} ->
                    monitor(process, NewClientProxy),
                    message_forwarder_loop(Socket, NewClientProxy)
            after infinity -> noop
            end;
        _ ->
             % the same recieve-0-and-listen-with-timeout trick here.
            receive
                % if the client quits on us, close the socket and then quit ourselves.
                {'DOWN', _ref, process, ClientProxy, _Reason} ->
                    gen_tcp:close(Socket),
                    exit(normal);
                {to_client, Message} ->
                    gen_tcp:send(Socket, Message)
            after 0 -> noop
            end,
            case gen_tcp:recv(Socket, 0) of
                {ok, Data} ->
                    client_proxy:from_client(ClientProxy, Data),
                    message_forwarder_loop(Socket, ClientProxy);
                {error, closed}  ->
                    exit(normal)
            end
    end.

%%% Testing, although there isn't much we can test programmatically.
test(Port) ->
    {ok, Pid0} = channel_supervisor:start_link(),
    Pid0 = whereis(channel_supervisor),
    {ok, Pid1} = channel_registry:start(),
    {ok, Pid2} = start(Port).
