%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc Cowboy Middleware that enables CORS-standard response headers.
%% @end
%%%-------------------------------------------------------------------
-module(enable_cors).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
  CorsHeaders = [
    {<<"Access-Control-Allow-Origin">>, <<"*">>},
    {<<"Access-Control-Allow-Headers">>, <<"Accept, Content-Type">>},
    {<<"Access-Control-Allow-Methods">>, <<"GET,HEAD,POST,DELETE,OPTIONS,PATCH">>}
  ],

  Req2 = lists:foldl(fun({Name, Value}, R) ->
    cowboy_req:set_resp_header(Name, Value, R)
  end, Req, CorsHeaders),

  {ok, Req2, Env}.
