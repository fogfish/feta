%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%%   lazy evaluation lazy http://www.serenethinking.com/2011/10/lazy-evaluation-and-lazy-in-erlang/
-module(lazy).

-export([
   new/2, advance/2, hd/1, tl/1, nth/2,
   filter/2, map/2, fold/2, fold/3, zip/3,
   build/1, list/2
]).
-compile({inline,[new/2]}).

%%
-type(lazy()   :: {any(), function()} | {}).

%%
-define(TESTS, tests).

%%
%% create lazy stream evaluation
-spec(new/2 :: (any(), function()) -> lazy()).

new(Head, Fun) ->
   {Head, Fun}.

%%
%% create new stream, next element is mapped by function from the previous one.
-spec(advance/2 :: (any(), function()) -> lazy()).

advance(Head, Fun) ->
   new(Head, fun() -> advance(Fun(Head), Fun) end).

%%
%% head element
-spec(hd/1 :: (lazy()) -> any()).

hd({Head, _}) ->
   Head.

%%
%% tail stream
-spec(tl/1 :: (lazy()) -> lazy()).

tl({_, Fun}) ->
   Fun().

%%
%% return nth element of stream
-spec(nth/2 :: (integer(), lazy()) -> lazy()).

nth(1, S) ->
   S;
nth(N, {_, Tail}) 
 when N >= 1 ->
  nth(N - 1, Tail()).

%%
%% return a new stream with filtered values.
-spec(filter/2 :: (function(), lazy()) -> lazy()).

filter(Pred, {Head, Tail}) ->
   case Pred(Head) of
      true ->
         new(Head, fun() -> filter(Pred, Tail()) end);
      false ->
         filter(Pred, Tail())
   end.

%%
%% returns a new stream with mapped values.
-spec(map/2 :: (function(), lazy()) -> lazy()).

map(Fun, {Head, Tail}) ->
   new(Fun(Head), fun() -> map(Fun, Tail()) end).


%%
%% fold / reduce stream elements based on predicated function
-spec(fold/2 :: (function(), lazy()) -> lazy()).

fold(Pred, S) ->
  acc(Pred, [], S).

acc(Pred, Acc, {Head, Tail}) ->
   case Pred(Head) of
      false ->
         new(lists:reverse([Head | Acc]), fun() -> fold(Pred, Tail()) end);
      true  ->
         acc(Pred, [Head | Acc], Tail())
   end.

%%
%% fold function over stream
-spec(fold/3 :: (function(), any(), lazy()) -> lazy()).

fold(Fun, Acc0, {Head, Tail}) ->
   Acc = Fun(Head, Acc0),
   new(Acc, fun() -> fold(Fun, Acc, Tail()) end).

%%
%% zips two streams
-spec(zip/3 :: (function(), lazy(), lazy()) -> lazy()).

zip(Fun, {HeadA, TailA}, {HeadB, TailB}) ->
   new(Fun(HeadA, HeadB), fun() -> zip(Fun, TailA(), TailB()) end).


%%
%% build lazy stream from erlang data types
-spec(build/1 :: (any()) -> lazy()).

build([]) ->
   new(undefined, fun() -> build([]) end);
build([H|T]) ->
   new(H, fun() -> build(T) end).

%%
%%
list(N, S) ->
   lists:reverse(
      lazy:hd(
         lazy:nth(N,
            lazy:fold(fun(X, Acc) -> [X | Acc] end, [], S)
         )
      )
   ).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").

%% integer lazy
int(X) -> X + 1.

%%
%%
map_test() ->
   %% erlang comprehensive 
   {T1, _} = timer:tc(
      fun() ->
         [X * 2 || X <- lists:seq(1, 100000)]
      end
   ),
   %% lists map
   {T2, _} = timer:tc(
      fun() ->
         lists:map(fun(X) -> X * 2 end, lists:seq(1, 100000))
      end
   ),
   %% lazy map
   {T3, _} = timer:tc(
      fun() ->
         lazy:nth(100000, 
            lazy:map(fun(X) -> X * 2 end, lazy:advance(1, fun int/1))
         )
      end
   ),
   error_logger:error_report([
      {comprehensive,   T1},
      {lists_map,       T2},
      {lazy_map,     T3}
   ]).

%%
%%
fold_test() ->
   %% lists fold
   R2 = timer:tc(
      fun() ->
         lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 100000))
      end
   ),
   %% lazy fold
   R3 = timer:tc(
      fun() ->
         lazy:nth(100000, 
            lazy:fold(fun(X, Acc) -> X + Acc end, 0, lazy:advance(1, fun int/1))
         )
      end
   ),
   error_logger:error_report([
      {lists_fold,    R2},
      {lazy_fold,     R3}
   ]).


-endif.


