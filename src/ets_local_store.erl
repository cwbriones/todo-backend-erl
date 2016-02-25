%%%-------------------------------------------------------------------
%% @author Christian Briones <cwbriones@gmail.com>
%% @doc An ETS-backed local store.
%% @end
%%%-------------------------------------------------------------------

-module(ets_local_store).

%% API exports
-export([
    create/2,
    delete/2,
    update/3,
    find/2,
    find_all/1,
    delete_all/1
  ]).

%% Internal exports
-export([
    start_link/1
  ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(TABLE(M), list_to_atom("ets_local_store_" ++ atom_to_list(M))).

%%==============================================================================
%% Public API
%%==============================================================================

create(Module, F) ->
  gen_server:call(Module, {create, F}).

update(Module, Id, F) ->
  gen_server:call(Module, {update, Id, F}).

delete(Module, Id) ->
  gen_server:cast(Module, {delete, Id}).

find(Module, Id) ->
  gen_server:call(Module, {find, Id}).

find_all(Module) ->
  gen_server:call(Module, find_all).

delete_all(Module) ->
  gen_server:cast(Module, delete_all).

%%==============================================================================
%% Gen Server Callbacks
%%==============================================================================

-spec start_link(atom()) -> {ok, Pid} | {error, {already_started, Pid}} | {error, term()}.
start_link(Module) ->
  gen_server:start_link({local, Module}, ?MODULE, [Module], []).

-type state() :: term().
-type handle_cast_result(State) :: {noreply, State}
  | {noreply, State, timeout()}
  | {noreply, State, hibernate}
  | {stop, Reason :: term(), State}.
-type handle_call_result(State) :: handle_cast_result(State)
  | {reply, Reply :: term(), State}
  | {reply, Reply :: term(), State, timeout()}
  | {reply, Reply :: term(), State, hibernate}
  | {stop, Reason :: term(), Reply :: term(), State}.

-spec init(term()) -> {ok, state()}.
init([Mod]) ->
  Tid = ?TABLE(Mod),
  Tid = ets:new(Tid, [public, named_table, set, {write_concurrency, true}]),
  State = #{tid => Tid},
  {ok, State}.

-spec handle_call(term(), term(), state()) -> handle_call_result(state()).
handle_call({create, F}, _, State = #{tid := Tid}) ->
  Id = create_uuid(),
  Item = F(Id),
  true = ets:insert(Tid, {Id, Item}),
  {reply, {ok, Item}, State};
handle_call({find, Id}, _, State = #{tid := Tid}) ->
  Reply = case ets:lookup(Tid, Id) of
    [] -> {error, notfound};
    [{Id, Item}] -> {ok, Item}
  end,
  {reply, Reply, State};
handle_call(find_all, _, State = #{tid := Tid}) ->
  All = [Item || {_Id, Item} <- ets:tab2list(Tid)],
  {reply, {ok, All}, State};
handle_call({update, Id, F}, _, State = #{tid := Tid}) ->
  Reply = case ets:lookup(Tid, Id) of
    [] -> {error, notfound};
    [{Id, Item}] ->
      Patched = F(Item),
      true = ets:insert(Tid, {Id, Patched}),
      {ok, Patched}
  end,
  {reply, Reply, State};
handle_call(Message, _, State) ->
  io:format("Unknown call ~p~n", [Message]),
  {reply, ok, State}.

-spec handle_cast(term(), state()) -> handle_cast_result(state()).
handle_cast({delete, Id}, State = #{tid := Tid}) ->
  ets:delete(Tid, Id),
  {noreply, State};
handle_cast(delete_all, State = #{tid := Tid}) ->
  ets:delete_all_objects(Tid),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> handle_cast_result(state()).
handle_info(_, State) ->
  {noreply, State}.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%%==============================================================================
%% Private API
%%==============================================================================

create_uuid() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

