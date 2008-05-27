%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc makes sure a torrent is downloaded
%% @end
%%%-------------------------------------------------------------------
-module(leech).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("records.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/1,
         status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {metainfo}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Metainfo) ->
    gen_server:start_link(?MODULE, [Metainfo], []).

status(Pid) ->
    gen_server:call(Pid, status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([Metainfo]) ->
    ?INFO("Starting a leech: ~p~n", [Metainfo#metainfo.id]),
    process_flag(trap_exit, true),
    State = #state{metainfo = Metainfo},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call(start, _From, State = #state{metainfo = Metainfo}) ->
    {ok, Pid} = tracker:start_link(self(), Metainfo),
    ?INFO("Starting tracker: ~p~n", [Pid]),
    {reply, ok, State};

handle_call(status, _From, State) ->
    {reply, {ok, State}, State};

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p~n", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
