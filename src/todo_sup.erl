%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc Todo top-level Supervisor. Manages HTTP server and ETS store.
%% @end
%%%-------------------------------------------------------------------

-module(todo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_SPEC(Mod), #{id => Mod, start => {Mod, start_link, []}}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, {{one_for_one, 0, 1}, [
    ?CHILD_SPEC(todo_app),
    ?CHILD_SPEC(todo_store)
  ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
