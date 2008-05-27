%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the main interface
%% @end
%%%-------------------------------------------------------------------
-module(herder).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("records.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0,
         launch/1,
         fetch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {leech_sup_pid, leech_set}).
-record(leech_child, {metainfo, pid}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

launch(Id) ->
    gen_server:call(?SERVER, {launch, Id}).

fetch(InfoHash) ->
    gen_server:call(?SERVER, {fetch, InfoHash}).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, Pid} = leech_sup:start_link(),
    State = #state{leech_set = [], leech_sup_pid = Pid},
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
handle_call({launch, Id}, _From, State = #state{leech_set = LeechSet}) ->
    {ok, Metainfo} = metainfo:read(Id),
    {ok, Pid} = leech_sup:launch(Metainfo),
    Child = #leech_child{metainfo = Metainfo, pid = Pid},
    NewState = State#state{leech_set = [Child | LeechSet]},
    {reply, {ok, Pid}, NewState};

handle_call({fetch, InfoHash}, _From, State = #state{leech_set = LeechSet}) ->
    Reply = find_info_hash(InfoHash, LeechSet),
    {reply, Reply, State};

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
find_info_hash(InfoHash, [LeechChild = #leech_child{metainfo = Metainfo, pid = Pid} | Rest]) ->
    case Metainfo#metainfo.info_hash of
        InfoHash ->
            {ok, Metainfo, Pid};
        _ ->
            find_info_hash(InfoHash, Rest)
    end;

find_info_hash(_InfoHash, []) ->
    not_found.
