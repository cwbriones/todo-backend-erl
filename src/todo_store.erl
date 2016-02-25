%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc An ETS-backed store for Todos.
%% @end
%%%-------------------------------------------------------------------

-module(todo_store).

%% API exports
-export([
    create/2,
    delete/1,
    update/2,
    find/1,
    find_all/0,
    delete_all/0
  ]).

%% Internal exports
-export([
    start_link/0
  ]).

%%==============================================================================
%% Public API
%%==============================================================================

start_link() ->
  ets_local_store:start_link(?MODULE).

create(Props, Url) ->
  ets_local_store:create(?MODULE, fun(Id) ->
      Defaults = #{completed => false, order => null, title => <<>>},
      FinalUrl = <<Url/binary, "/", Id/binary>>,
      maps:merge(Defaults, Props#{id => Id, url => FinalUrl})
    end).

update(Id, Patch) ->
  ets_local_store:update(?MODULE, Id, fun(Todo) ->
      maps:merge(Todo, Patch)
    end).

delete(Id) ->
  ets_local_store:delete(?MODULE, Id).

find(Id) ->
  ets_local_store:find(?MODULE, Id).

find_all() ->
  ets_local_store:find_all(?MODULE).

delete_all() ->
  ets_local_store:delete_all(?MODULE).
