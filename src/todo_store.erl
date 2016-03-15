%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc An ETS-backed store for Todos.
%% @end
%%%-------------------------------------------------------------------

-module(todo_store).

%% API exports
-export([
    create/2,
    update/2,
    find/1,
    find_by/1,
    find_all/0,
    delete/1,
    delete_by/1,
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

find(Id) ->
  ets_local_store:find(?MODULE, Id).

find_by(Props) ->
  ets_local_store:find_by(?MODULE, Props).

find_all() ->
  ets_local_store:find_all(?MODULE).

delete(Id) ->
  ets_local_store:delete(?MODULE, Id).

delete_by(Props) ->
  ets_local_store:delete_by(?MODULE, Props).

delete_all() ->
  ets_local_store:delete_all(?MODULE).
