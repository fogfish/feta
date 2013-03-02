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
%%   lazy stream evaluation http://www.serenethinking.com/2011/10/lazy-evaluation-and-lazy-in-erlang/
-module(lazy).

-export([
   new/0, new/1, new/2, advance/2, hd/1, tl/1, nth/2,
   filter/2, map/2, fold/2, fold/3, mapfold/3, zip/3,
   build/1, list/2
]).

%%
-type(lazy()   :: {any(), function()}).

%%
-compile({inline,[new/0, new/1, new/2, hd/1, tl/1]}).
-define(TESTS, tests).

%%
%% create lazy stream evaluation
-spec(new/2 :: (any(), function()) -> lazy()).

new() ->
   {}.

new(Head) ->
   {Head, fun() -> new() end}.

new(eof, _Fun) ->
   {};
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
   end;
filter(_, {}) ->
   {}.

%%
%% returns a new stream with mapped values.
-spec(map/2 :: (function(), lazy()) -> lazy()).

map(Fun, {Head, Tail}) ->
   new(Fun(Head), fun() -> map(Fun, Tail()) end);
map(_, {}) ->
   {}.


%%
%% fold / reduce stream elements based on predicated function
-spec(fold/2 :: (function(), lazy()) -> lazy()).

fold(Pred, S) ->
  acc(Pred, [], S).

acc(Pred, Acc, {Head, Tail}) ->
   case Pred(Head, Acc) of
      false ->
         new(lists:reverse([Head | Acc]), fun() -> fold(Pred, Tail()) end);
      true  ->
         acc(Pred, [Head | Acc], Tail())
   end;
acc(_Pred, Acc, {}) ->
   new(lists:reverse(Acc), fun() -> new() end).

%%
%% fold function over stream
-spec(fold/3 :: (function(), any(), lazy()) -> lazy()).

fold(Fun, Acc0, {Head, Tail}) ->
   Acc = Fun(Head, Acc0),
   new(Acc, fun() -> fold(Fun, Acc, Tail()) end);
fold(_, _, {}) ->
   {}.

%%
%% mapfold function over stream
-spec(mapfold/3 :: (function(), any(), lazy()) -> lazy()).

mapfold(Fun, Acc0, {Head, Tail}) ->
   {Val, Acc} = Fun(Head, Acc0),
   new(Val, fun() -> mapfold(Fun, Acc, Tail()) end);
mapfold(_, _, {}) ->
   {}.


%%
%% zips two streams
-spec(zip/3 :: (function(), lazy(), lazy()) -> lazy()).

zip(Fun, {HeadA, TailA}, {HeadB, TailB}) ->
   new(Fun(HeadA, HeadB), fun() -> zip(Fun, TailA(), TailB()) end);
zip(_, {}, _) ->
   {};
zip(_, _, {}) ->
   {}.


%%
%% build lazy stream from erlang data types
-spec(build/1 :: (any()) -> lazy()).

build([]) ->
   new();
build([H|T]) ->
   new(H, fun() -> build(T) end).

%%
%%
list(N, S) ->
   lists:reverse(
      lazy:hd(
         lazy:nth(
            N,
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

lazy_stream_test() ->
   S   = lazy:new(1, fun() -> lazy:new(2) end),
   1   = lazy:hd(S),
   2   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   2   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).
 
lazy_advance_test() ->
   S = lazy:advance(1, fun(X) when X < 2 -> X + 1; (_) -> eof end),
   1   = lazy:hd(S),
   2   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   2   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_filter_test() ->
   S = lazy:filter(
      fun(X) -> X rem 2 =:= 0 end,
      lazy:advance(1, fun(X) when X < 5 -> X + 1; (_) -> eof end)
   ),

   2   = lazy:hd(S),
   4   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   2   = lazy:hd(lazy:nth(1, S)),
   4   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_map_test() ->
   S = lazy:map(
      fun(X) -> X * X end,
      lazy:advance(1, fun(X) when X < 2 -> X + 1; (_) -> eof end)
   ),

   1   = lazy:hd(S),
   4   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   4   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).


lazy_fold_test() ->
   S = lazy:fold(
      fun(X, _) -> X rem 2 =:= 1 end,
      lazy:advance(1, fun(X) when X < 5 -> X + 1; (_) -> eof end)
   ),

   [1, 2] = lazy:hd(S),
   [3, 4] = lazy:hd(lazy:tl(S)),
   [5]    = lazy:hd(lazy:tl(lazy:tl(S))),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(lazy:tl(S))))),

   [1, 2] = lazy:hd(lazy:nth(1, S)),
   [3, 4] = lazy:hd(lazy:nth(2, S)),
   [5]    = lazy:hd(lazy:nth(3, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(4, S))).

lazy_fold_acc_test() ->
   S = lazy:fold(
      fun(X, Acc) -> Acc + X end,
      10,
      lazy:advance(1, fun(X) when X < 2 -> X + 1; (_) -> eof end)
   ),

   11  = lazy:hd(S),
   13  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   11  = lazy:hd(lazy:nth(1, S)),
   13  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_mapfold_test() ->
   S = lazy:mapfold(
      fun(X, Acc) -> {Acc + X, Acc} end,
      10,
      lazy:advance(1, fun(X) when X < 2 -> X + 1; (_) -> eof end)
   ),

   11  = lazy:hd(S),
   12  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   11  = lazy:hd(lazy:nth(1, S)),
   12  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_zip_test() ->
   S = lazy:zip(
      fun(A, B) -> A + B end,
      lazy:advance(10, fun(X) when X < 20 -> X + 10; (_) -> eof end),
      lazy:advance(1,  fun(X) when X < 2  -> X +  1; (_) -> eof end)
   ),

   11  = lazy:hd(S),
   22  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   11  = lazy:hd(lazy:nth(1, S)),
   22  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_build_test() ->
   S   = lazy:build([1, 2]),
   1   = lazy:hd(S),
   2   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   2   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))),

   [1]    = lazy:list(1, S),
   [1, 2] = lazy:list(2, S),
   {'EXIT', _} = (catch lazy:list(3, S)).

%%
%%
lazy_perf_test() ->
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
            lazy:map(fun(X) -> X * 2 end, lazy:advance(1, fun(X) -> X + 1 end))
         )
      end
   ),
   %% lists fold
   {T4, _} = timer:tc(
      fun() ->
         lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 100000))
      end
   ),
   %% lazy fold
   {T5, _} = timer:tc(
      fun() ->
         lazy:nth(100000, 
            lazy:fold(fun(X, Acc) -> X + Acc end, 0, lazy:advance(1, fun(X) -> X + 1 end))
         )
      end
   ),
   error_logger:error_report([
      {comprehensive,   T1},
      {lists_map,       T2},
      {lazy_map,        T3},
      {lists_fold,      T4},
      {lazy_fold,       T5}
   ]).

-endif.


