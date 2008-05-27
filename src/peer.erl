%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <dev@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the interaction between here and a tracker
%% @end
-module(peer).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([waiting_for_socket/2, waiting_for_socket/3, 
         handshaking/2, handshaking/3]).

-record(state, {socket, buffer, leech_pid}).

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
    gen_fsm:start_link(?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, waiting_for_socket, #state{buffer = <<>>}}.

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
    ?INFO("made connection: ~p~n", [Socket]),
    inet:setopts(Socket, [{active, true}, {packet, raw}, binary]),
    NewState = State#state{socket = Socket},
    {next_state, handshaking, NewState}.

handshaking({data, <<19:8,?PROTOCOL_STRING,Reserved:8/binary,InfoHash:20/binary,Rest/binary>>}, State = #state{socket = Socket, buffer = Buffer}) ->
    ?INFO("got InfoHash: ~s~n", [InfoHash]),
    deliver_handshake(Socket, InfoHash),
    {next_state, handshaking, State#state{buffer = list_to_binary([Buffer, Rest])}};

handshaking({data, Data}, State = #state{socket = Socket, buffer = Buffer}) ->
    {next_state, handshaking, State#state{buffer = list_to_binary([Buffer, Data])}}.

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
waiting_for_socket(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handshaking(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
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
handle_info({tcp, Socket, Data}, StateName, State = #state{socket = Socket}) ->
    gen_fsm:send_event(self(), {data, Data}),
    {next_state, StateName, State};

handle_info({tcp_closed, Socket}, StateName, State = #state{socket = Socket}) ->
    ?INFO("socket is closed: ~p~n", [Socket]),
    gen_fsm:send_event(self(), tcp_closed),
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
deliver_handshake(Socket, InfoHash) ->
    gen_tcp:send(Socket, <<19,?PROTOCOL_STRING,?RESERVED_BYTES,InfoHash,"T03I-----Pnku-i7sfPF">>).
