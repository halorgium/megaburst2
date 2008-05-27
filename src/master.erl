%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc keep an eye on the slaves
%% @end
%%%-------------------------------------------------------------------
-module(master).

-behaviour(supervisor).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0]).

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
    Children = [{"herder",
                 {herder,start_link,[]},
                 permanent,2000,worker,
                 [herder]
                },
                {"peer_sup",
                 {gen_socket_listener_sup,start_link,[clients, "clients", 9999, peer, []]},
                 permanent,2000,supervisor,
                 [gen_socket_listener_sup]
                }],
    {ok,{{one_for_one,0,1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
