%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc keep an eye on the slaves
%% @end
%%%-------------------------------------------------------------------
-module(leech_sup).

-behaviour(supervisor).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0,
         launch/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

launch(Filename) ->
    Spec = leech_master_spec(Filename),
    ?INFO("Wanting to spawn a leech master: ~p~n", [Spec]),
    {ok, Pid} = supervisor:start_child(?SERVER, Spec),
    gen_server:call(Pid, start),
    {ok, Pid}.


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,1,60}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
leech_master_spec(Filename) ->
    {{leech, make_ref()},
     {leech,start_link,[Filename]},
     permanent,2000,worker,
     [leech]
    }.
