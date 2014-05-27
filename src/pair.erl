%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   key / val pair helper utility
-module(pair).

-export([
   lookup/2
  ,lookup/3
  ,check/3
  ,maybe/3
]).

-type(key()   :: atom() | binary()).
-type(val()   :: any()).
-type(pairs() :: [{key(), val()}]).


%%
%% lookup value using path
%% throws exception if value do not exists
-spec(lookup/2 :: ([key()], pairs()) -> val()).

lookup([Key | Tail], Value) ->
   lookup(Tail, lookup_term(Key, Value));
lookup([], Value) ->
   Value;
lookup(Key, Value) ->
   {Key, Val} = lists:keyfind(Key, 1, Value),
   Val.

%%
%% lookup value using path
%% return default value if path do not exists
-spec(lookup/3 :: ([key()], val(), pairs()) -> val()).

lookup(Path, Default, Value) ->
   try
      lookup(Path, Value)
   catch _:_ ->
      Default
   end.

lookup_term(_, undefined) ->
   undefined;
lookup_term(Key, X)
 when is_integer(Key), is_tuple(X) ->
   erlang:element(Key, X);
lookup_term(Key, X)
 when is_integer(Key), is_list(X) ->
   lists:nth(Key, X);
lookup_term(Key, X)
 when is_list(X) ->
   case lists:keyfind(Key, 1, X) of
      false    -> exit({badarg, Key});
      {_, Val} -> Val
   end.

%%
%% check that value is defined
-spec(check/3 :: (val() | function(), key(), pairs()) -> ok).

check(Val, Key, Pairs) ->
   assert(Val, Key, lookup(Key, Pairs)).

%%
%% check that value is defined
-spec(maybe/3 :: (val() | function(), key(), pairs()) -> ok).

maybe(Val, Key, Pairs) ->
   assert(Val, Key, lookup(Key, undefined, Pairs)).

%%
%%
assert(_Val, _Key, undefined) ->
   ok;
assert(Val, _Key, Val) ->
   ok;
assert(Val, Key, _Any) ->
   exit({badarg, Key, Val}).
