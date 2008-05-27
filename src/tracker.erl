%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <dev@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the interaction between here and a tracker
%% @end
-module(tracker).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("records.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/2, announce/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([idle/2]).

-record(state, {leech_pid, metainfo, request_id}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(LeechPid, Metainfo) ->
    gen_fsm:start_link(?MODULE, [LeechPid, Metainfo], []).

announce(Pid) ->
    gen_fsm:send_event(Pid, announce).

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
init([LeechPid, Metainfo]) ->
    {ok, idle, #state{leech_pid = LeechPid, metainfo = Metainfo}}.

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
idle(announce, State = #state{metainfo = Metainfo}) ->
    Url = announce_url(Metainfo),
    ?INFO("Url: ~p~n", [Url]),
    {ok, RequestId} = http:request(get, {Url, []}, [], [{sync, false}]),
    ?INFO("RequestId: ~p~n", [RequestId]),
    {next_state, loading, State#state{request_id = RequestId}}.

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
%%              NextState} |
%%                                          {next_state, NextStateName, 
%%                    NextState, Timeout} |
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
handle_info({http, {RequestId, {{Version, 200, ReasonPhrase}, Headers, Body}}}, loading, State = #state{leech_pid = LeechPid, request_id = RequestId}) ->
    ?INFO("Got body: ~p~n", [Body]),
    {ok, Term} = bencoding:decode(Body),
    ?INFO("Got term: ~p~n", [Term]),
    {ok, Complete}    = bencoding:search_dict("complete", Term),
    {ok, Incomplete}  = bencoding:search_dict("incomplete", Term),
    {ok, Interval}    = bencoding:search_dict("interval", Term),
    {ok, PeersString} = bencoding:search_dict("peers", Term),
    PeersBinary       = list_to_binary(PeersString),
    {ok, Peers}       = parse_peers(PeersBinary, []),
    TrackerData = #tracker_data{complete   = Complete,
                                incomplete = Incomplete,
                                interval   = Interval,
                                peers      = Peers},
    ?INFO("TrackerData: ~p~n", [TrackerData]),
    ok = leech:update(LeechPid, TrackerData),
    {next_state, idle, State#state{request_id = undefined}};

handle_info(Info, StateName, State) ->
    ?INFO("Unexpected info msg ~p in state ~p (~p)~n", [Info, StateName, State]),
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
announce_url(#metainfo{peer_id = PeerId, info_hash = InfoHash, announce = Announce}) ->
    Params = [{"info_hash", InfoHash},
              {"peer_id", PeerId},
              {"compact", "1"},
              {"event", "started"},
              {"uploaded", "0"},
              {"downloaded", "0"},
              {"left", "0"},
              {"port", "9999"}],
    EncodedParams = encode_params(Params),
    lists:flatten(io_lib:format("~s?~s", [Announce, EncodedParams])).

encode_params(Params) ->
    lists:flatten(encode_params(Params, [])).

encode_params([{Key,Value}], EncodedParams) ->
    Str = encode_key_value(Key, Value),
    NewEncodedParams = [Str | EncodedParams],
    lists:reverse(NewEncodedParams);

encode_params([{Key,Value} | Rest], EncodedParams) ->
    Str = encode_key_value(Key, Value),
    encode_params(Rest, ["&", Str | EncodedParams]).

encode_key_value(Key, Value) ->
    lists:flatten(io_lib:format("~s=~s", [encode_param(Key), encode_param(Value)])).

encode_param(String) when is_list(String) ->
    ibrowse_lib:url_encode(String);

encode_param(Binary) when is_binary(Binary) ->
    encode_param(binary_to_list(Binary)).

parse_peers(<<A:8,B:8,C:8,D:8,Port:16,Rest/binary>>, PeersList) ->
    Peer = {{A, B, C, D}, Port},
    case lists:member(Peer, PeersList) of
        true ->
            ?INFO("Found a duplicate peer: ~p~n", [Peer]),
            parse_peers(Rest, PeersList);
        false ->
            ?INFO("Found a new peer: ~p~n", [Peer]),
            parse_peers(Rest, [Peer | PeersList])
    end;

parse_peers(<<>>, PeersList) ->
    {ok, lists:reverse(PeersList)}.

%% Tests
encode_param_test() ->
    "foo" = encode_param("foo").
encode_params_test() ->
    "foo=bar" = encode_params([{"foo", "bar"}]).
encode_params_2_test() ->
    "foo=bar&baz=qux" = encode_params([{"foo", "bar"}, {"baz", "qux"}]).
announce_url_test() ->
    "foo=bar&baz=qux" = announce_url(metainfo).
