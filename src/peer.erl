%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <dev@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the interaction between here and a tracker
%% @end
-module(peer).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("records.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0, connect/4, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([waiting_for_socket/2,
         idle/2]).

-record(state, {buffer, socket, leech_pid, metainfo}).

-define(PROTOCOL_STRING, "BitTorrent protocol").
-define(RESERVED_BYTES, 0:64/big).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [waiting_for_socket], []).

connect(LeechPid, Metainfo, Address, Port) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [idle], []),
    ok = gen_fsm:send_event(Pid, {connect, LeechPid, Metainfo, Address, Port}),
    {ok, Pid}.

% This is called by the gen_socket_listener process to
% pass the socket that a new connection process will be
% responsible for.
%
% @set_socket(Pid, Socket) -> void
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([InitialStateName]) ->
    {ok, InitialStateName, #state{}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
waiting_for_socket({socket_ready, Socket}, State) ->
    ?INFO("got incoming connection: ~p~n", [Socket]),
    inet:setopts(Socket, [{active, true}, {packet, raw}, binary]),
    {next_state, handshaking, State#state{socket = Socket}}.

idle({connect, LeechPid, Metainfo, Address, Port}, State) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, [{active, true}, {packet, raw}, binary]),
    ok = gen_tcp:send(Socket, handshake_for(Metainfo#metainfo.info_hash)),
    {next_state, sent_handshake, State#state{socket = Socket, leech_pid = LeechPid, metainfo = Metainfo}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%                                           NextState} |
%%                                          {next_state, NextStateName, 
%%                                           NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, StateName, State = #state{buffer = Buffer}) when is_binary(Buffer) ->
    handle_info({tcp, Socket, list_to_binary([Buffer, Data])}, StateName, State#state{buffer = undefined});

handle_info({tcp, Socket, Data}, StateName, State) ->
    ?INFO("Got data: ~p~n", [Data]),
    consume_data(StateName, Data, State, Socket);

handle_info({tcp_closed, Socket}, StateName, State = #state{socket = Socket}) ->
    ?INFO("socket is closed: ~p~n", [Socket]),
    {next_state, StateName, State};

handle_info(Info, StateName, State) ->
    ?INFO("Unexpected info msg ~p in state ~p~n", [Info, StateName]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
consume_data(StateName, Data, State, Socket) ->
    case handle_data(StateName, Data, State) of
        incomplete ->
            ?WARN("incomplete data in ~p: ~p~n", [StateName, Data]),
            {next_state, StateName, State#state{buffer = Data}};
        {noreply, NewStateName, Rest, NewState} when is_atom(NewStateName) ->
            consume_data(NewStateName, Rest, NewState, Socket);
        {reply, NewStateName, ReplyData, Rest, NewState} when is_atom(NewStateName) ->
            ok = gen_tcp:send(Socket, ReplyData),
            consume_data(NewStateName, Rest, NewState, Socket);
        E ->
            {stop, {handle_data_fail, E}, State}
    end.

handle_data(sent_handshake, <<19:8,?PROTOCOL_STRING,Reserved:8/binary,InfoHash:20/binary,RemotePeerId:20/binary,Rest/binary>>, State = #state{metainfo = Metainfo}) ->
    ?INFO("got InfoHash: ~s, PeerId: ~s~n", [InfoHash, RemotePeerId]),
    InfoHash = Metainfo#metainfo.info_hash,
    PeerId = Metainfo#metainfo.peer_id,
    {reply, running, PeerId, Rest, State};

handle_data(handshaking, <<19:8,?PROTOCOL_STRING,Reserved:8/binary,InfoHash:20/binary,Rest/binary>>, State) ->
    ?INFO("got InfoHash: ~s~n", [InfoHash]),
    case herder:fetch(InfoHash) of
        {ok, LeechPid, Metainfo} ->
            ?INFO("valid InfoHash~n", []),
            {reply, waiting_for_peer_id, [handshake_for(Metainfo#metainfo.info_hash), Metainfo#metainfo.peer_id], Rest, State#state{leech_pid = LeechPid, metainfo = Metainfo}};
        not_found ->
            {no_reply, info_hash_missing, <<>>, State}
    end;

handle_data(waiting_for_peer_id, <<PeerId:20/binary,Rest/binary>>, State) ->
    ?INFO("got PeerId: ~s~n", [PeerId]),
    {noreply, running, Rest, State};

handle_data(running, <<0:32,Rest/binary>>, State) ->
    ?INFO("got keepalive~n", []),
    {noreply, running, Rest, State};

handle_data(running, <<1:32,0,Rest/binary>>, State) ->
    ?INFO("got choke~n", []),
    {noreply, running, Rest, State};

handle_data(running, <<1:32,1,Rest/binary>>, State) ->
    ?INFO("got unchoke~n", []),
    {noreply, running, Rest, State};

handle_data(running, <<1:32,2,Rest/binary>>, State) ->
    ?INFO("got interested~n", []),
    {noreply, running, Rest, State};

handle_data(running, <<1:32,3,Rest/binary>>, State) ->
    ?INFO("got not interested~n", []),
    {noreply, running, Rest, State};

handle_data(running, <<5:32,4,PieceIndex:4/binary,Rest/binary>>, State) ->
    ?INFO("got have: PieceIndex: ~p~n", [PieceIndex]),
    {noreply, running, Rest, State};

handle_data(running, <<BitFieldLen:32,5,BitFieldPlusOne:BitFieldLen/binary,Rest/binary>>, State) ->
    {BitField, PlusOne} = split_binary(BitFieldPlusOne, BitFieldLen - 1),
    ?INFO("got BitField: ~p~n", [BitField]),
    {noreply, running, list_to_binary([PlusOne, Rest]), State};

handle_data(_StateName, _Data, _State) ->
    incomplete.

handshake_for(InfoHash) ->
    [19, ?PROTOCOL_STRING, <<?RESERVED_BYTES>>, InfoHash].
