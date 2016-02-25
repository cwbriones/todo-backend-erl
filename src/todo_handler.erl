%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc Todo Cowboy Handler
%% @end
%%%-------------------------------------------------------------------

-module(todo_handler).

-behaviour(cowboy_handler).
-export([init/2, parse_with_keys/2]).

init(Req, Opts = [Handler]) ->
  Method = cowboy_req:method(Req),
  Req2 = handle(Req, Method, Handler),
  {ok, Req2, Opts}.

handle(Req, <<"GET">>, todos) ->
  Id = cowboy_req:binding(id, Req),
  case todo_store:find(Id) of
    {error, notfound} -> not_found(Req);
    {ok, Todo} -> json(Todo, Req)
  end;
handle(Req, <<"PATCH">>, todos) ->
  %% Update a todo
  Id = cowboy_req:binding(id, Req),
  {ok, Patch, Req2} = parse_with_keys([completed, title, order], Req),
  case todo_store:update(Id, Patch) of
    {ok, NewTodo} -> json(NewTodo, Req2);
    {error, notfound} -> not_found(Req2)
  end;
handle(Req, <<"DELETE">>, todos) ->
  %% Delete a todo
  Id = cowboy_req:binding(id, Req),
  todo_store:delete(Id),
  ok(Req);
handle(Req, <<"POST">>, all_todos) ->
  %% Create a todo
  {ok, Props, Req2} = parse_with_keys([title, order], Req),
  {ok, Todo = #{id := Id}} = todo_store:create(Props, <<"http://127.0.0.1:8080/todos">>),
  Req3 = cowboy_req:set_resp_header(<<"Location">>, <<"/todos/", Id/binary>>, Req2),
  json(Todo, Req3);
handle(Req, <<"GET">>, all_todos) ->
  {ok, Todos} = todo_store:find_all(),
  json(Todos, Req);
handle(Req, <<"DELETE">>, all_todos) ->
  todo_store:delete_all(),
  ok(Req);
handle(Req, <<"OPTIONS">>, _) ->
  ok(Req);
handle(Req, Method, _) ->
  Path = cowboy_req:path(Req),
  Response = <<"Method ", Method/binary, " not allowed for ", Path/binary>>,
  cowboy_req:reply(405, [], Response, Req).

%%===================================================================
%% Utilities
%%===================================================================

ok(Req) ->
  cowboy_req:reply(200, Req).

not_found(Req) ->
  cowboy_req:reply(404, [], <<"Not Found">>, Req).

parse_with_keys(Keys, Req) ->
  case cowboy_req:body(Req) of
    {ok, Body, Req2} ->
      Decoded = jsx:decode(Body, [return_maps]),
      Parsed = lists:foldl(fun
        ({K, Default}, Acc) ->
          BinKey = atom_to_binary(K, utf8),
          V = maps:get(BinKey, Decoded, Default),
          maps:put(K, V, Acc);
        (K, Acc) ->
          BinKey = atom_to_binary(K, utf8),
          V = maps:get(BinKey, Decoded, null),
          maps:put(K, V, Acc)
      end, #{}, Keys),
      {ok, Parsed, Req2};
    Err -> Err
  end.

json(Term, Req) ->
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Filtered = prefilter_null(Term),
  Encoded = jsx:encode(Filtered),
  cowboy_req:reply(200, Headers, Encoded, Req).

prefilter_null(Json) when is_map(Json) ->
  maps:fold(fun
    (_K, null, Acc) -> Acc;
    (K, V, Acc) -> maps:put(K, V, Acc)
  end, #{}, Json);
prefilter_null(Json) -> Json.

