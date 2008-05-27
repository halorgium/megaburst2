%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <dev@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the interaction between here and a tracker
%% @end
-module(tracker).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([offline/2, offline/3,
         loading/2, loading/3]).

-record(state, {leech, torrent}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(LeechPid, Torrent) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [LeechPid, Torrent], []),
    gen_fsm:send_event(Pid, load),
    {ok, Pid}.

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
init([LeechPid, Torrent]) ->
    {ok, offline, #state{leech = LeechPid, torrent = Torrent}}.

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
offline(load, State = #state{torrent = Torrent}) ->
    Url = announce_url("foo"),
    ?INFO("Url: ~p~n", [Url]),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = http:request(get, {Url, []}, [], []),
    {next_state, loading, State}.

loading(Event, State) ->
    ?INFO("Got event: ~p~n", [Event]),
    {next_state, loading, State}.

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
offline(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

loading(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

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
announce_url(M) ->
    Params = [{"info_hash",
               "X%86%C3B%02%E6%3D%18Y%D4%FCb%E1%DB%0B%F70%B6%40%8A"},
              {"peer_id",
               "T03I-----Pnku-i7sfPF"},
              {"compact", "1"},
              {"event", "started"},
              {"uploaded", "0"},
              {"downloaded", "0"},
              {"left", "0"},
              {"port", "9999"}],
    EncodedParams = encode_params(Params),
    Url = "http://localhost:8080/announce",
    lists:flatten(io_lib:format("~s?~s", [Url, EncodedParams])).

encode_params(Params) ->
    encode_params(Params, []).

encode_params([{Key,Value}], EncodedParams) ->
    Str = encode_param(Key, Value),
    NewEncodedParams = [Str | EncodedParams],
    lists:reverse(NewEncodedParams);

encode_params([{Key,Value} | Rest], EncodedParams) ->
    Str = encode_param(Key, Value),
    encode_params(Rest, ["&", Str | EncodedParams]).

encode_param(Key, Value) ->
    lists:flatten(io_lib:format("~s=~s", [Key, Value])).

%% Tests
encode_param_test() ->
    "foo=bar" = encode_param("foo", "bar").
encode_params_test() ->
    "foo=bar" = encode_params([{"foo", "bar"}]).
encode_params_2_test() ->
    "foo=bar&baz=qux" = encode_params([{"foo", "bar"}, {"baz", "qux"}]).
announce_url_test() ->
    "foo=bar&baz=qux" = announce_url(aa).
