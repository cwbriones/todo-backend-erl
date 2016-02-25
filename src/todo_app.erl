%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc Todo Cowboy HTTP Server
%% @end
%%%-------------------------------------------------------------------

-module(todo_app).

-export([start_link/0]).

%% Application callbacks
-behaviour(application).
-export([start/2, stop/1]).

-behaviour(cowboy_handler).
-export([init/2]).

-define(WEB_PORT, 8080).
-define(ACCEPTORS, 100).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  todo_sup:start_link().

start_link() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/todos", todo_handler, [all_todos]},
      {"/todos/:id", todo_handler, [todos]},
      {'_', ?MODULE, []}
    ]}
  ]),
  TransOpts = [{port, ?WEB_PORT}],
  ProtoOpts = [
    {timeout, 60000},
    {env, [{dispatch, Dispatch}]},
    {middlewares, [enable_cors, cowboy_router, cowboy_handler]}
  ],
  cowboy:start_http(http, ?ACCEPTORS, TransOpts, ProtoOpts).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

init(Req, Opts) ->
  Req2 = cowboy_req:reply(404, [], <<"Not found">>, Req),
  {ok, Req2, Opts}.
