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
-define(TESTS, tests).

-export([
   new/0, new/1, new/2, advance/2, hd/1, tl/1, nth/2, dropwhile/2,
   filter/2, map/2, fold/2, fold/3, unfold/2, unfold/3,
   mapfold/2, mapfold/3, zip/3, zip/2, interleave/3,
   build/1, list/2, list/1
]).

%%
%% lazy stream 
-type head()     :: any().
-type lazy()     :: {any(), function()}.      
-type predf()    :: fun((head()) -> true | false). 
-type mapf()     :: fun((head()) -> head()). 
-type foldf()    :: fun((head(),  any()) -> head()).
-type unfoldf()  :: fun((head(),  any()) -> {head(), any()}).
-type zipf()     :: fun((head(), head()) -> head()). 
-export_type([lazy/0]).

-compile({inline,[new/0, new/1, new/2, hd/1, tl/1]}).

%%
%% create lazy stream evaluation
-spec(new/0 :: () -> lazy()).
-spec(new/1 :: (any()) -> lazy()).
-spec(new/2 :: (any(), function()) -> lazy()).

new() ->
   {}.

new(Head) ->
   new(Head, fun lazy:new/0).

new(eof, _) ->
   new();
new(Head, Fun) ->
   {Head, Fun}.

%%
%% create new stream, next element is mapped by function from the previous one.
-spec(advance/2 :: (any(), function()) -> lazy()).

advance(Head, Fun) ->
   new(Head, fun() -> advance(Fun(Head), Fun) end).

%%
%% head element of stream
-spec(hd/1 :: (lazy()) -> any()).

hd({Head, _}) ->
   Head.

%%
%% tail of stream
-spec(tl/1 :: (lazy()) -> lazy()).

tl({_, Fun}) ->
   Fun().

%%
%% returns the suffix of the lazy stream that starts at the next element after the first n elements.
-spec(nth/2 :: (integer(), lazy()) -> lazy()).

nth(1, S) ->
   S;
nth(N, {_, Tail}) 
 when N >= 1 ->
  nth(N - 1, Tail()).

%%
%% returns the suffix of the input stream that starts at the first element x for which predicate is false.
-spec(dropwhile/2 :: (predf(), lazy()) -> lazy()).

dropwhile(Pred, {Head, Tail}) ->
   case Pred(Head) of
      true  -> dropwhile(Pred, Tail()); 
      false -> {Head, Tail}
   end;
dropwhile(_, {}) ->
   {}.


%%
%% returns a newly-allocated stream that contains only those elements x of the input stream for which predicate is true.
-spec(filter/2 :: (predf(), lazy()) -> lazy()).

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
%% returns a newly-allocated stream containing elements that are the results of map function
-spec(map/2 :: (mapf(), lazy()) -> lazy()).

map(Map, {Head, Tail}) ->
   new(Map(Head), fun() -> map(Map, Tail()) end);
map(_, {}) ->
   {}.


%%
%% returns a newly allocated stream containing elements that result of fold function. The fold
%% procedure takes stream head and accumulator, producing a new accumulator that becomes head of
%% stream (accumulates the partial folds). When no initial value is supplied then stream head
%% is used as initial value 
-spec(fold/2 :: (foldf(), lazy()) -> lazy()).
-spec(fold/3 :: (foldf(), any(), lazy()) -> lazy()).

fold(Fold, {Head, Tail}) ->
   fold(Fold, Head, Tail()).

fold(Fold, Acc0, {Head, Tail}) ->
   Acc = Fold(Head, Acc0),
   new(Acc, fun() -> fold(Fold, Acc, Tail()) end);
fold(_Fold, _Acc0, {}) ->
   {}.

%%
%% Unfold is dual to fold (recursive stream constructor). It take a "seed" value and apply a higher 
%% order function recursively to decide how to progressively re-construct a stream. The function
%% returns N newly allocated stream elements 
-spec(unfold/2 :: (unfoldf(), lazy()) -> lazy()).
-spec(unfold/3 :: (unfoldf(), any(), lazy()) -> lazy()).

unfold(UnFold, {Head, Tail}) ->
   unfold(UnFold, Head, Tail()).

unfold(UnFold, Acc0, {Head, Tail}) ->
   case UnFold(Head, Acc0) of
      % new seed and stream elements are returned
      {[H|T], Acc} -> new(H, fun() -> unfold1(T, UnFold, Acc, Tail) end);
      % no values are produced
      {[],    Acc} -> unfold(UnFold, Acc, Tail());
      % single value is produced
      {Val,   Acc} -> new(Val, fun() -> unfold(UnFold, Acc, Tail()) end);
      % end of stream is reached
      eof          -> {};
      % no values are produces
      Acc          -> unfold(UnFold, Acc, Tail())
   end;
unfold(_Fold, _Acc0, {}) ->
   {}.

unfold1([],    UnFold, Acc0, Tail) ->
   unfold(UnFold, Acc0, Tail());
unfold1([H|T], UnFold, Acc0, Tail) ->
   new(H, fun() -> unfold1(T, UnFold, Acc0, Tail) end).


%%
%% Map/Fold takes "seed" value and folds higher order function on stream to produce a new stream.
%% The function decides how to progressively re-construct a stream, replacing head values with the results of applying
%% a combining function and accumulator.
-spec(mapfold/2 :: (unfoldf(), lazy()) -> lazy()).
-spec(mapfold/3 :: (unfoldf(), any(), lazy()) -> lazy()).

mapfold(MapFold, {Head, Tail}) ->
   mapfold(MapFold, Head, Tail()).

mapfold(MapFold, Acc0, {Head, Tail}) ->
   case MapFold(Head, Acc0) of
      {Val, Acc} -> new(Val, fun() -> mapfold(MapFold, Acc, Tail()) end);
      eof        -> {};
      Acc        -> mapfold(MapFold, Acc, Tail())
   end;

mapfold(Fun, Acc0, {}) ->
   case Fun(eof, Acc0) of
      {Val, _}   -> new(Val, fun() -> new() end);
      _          -> new()
   end.

%%
%% Zip or convolution function that maps multiple streams to single one
-spec(zip/3 :: (zipf(), lazy(), lazy()) -> lazy()).

zip(Zip, {HeadA, TailA}, {HeadB, TailB}) ->
   new(Zip(HeadA, HeadB), fun() -> zip(Zip, TailA(), TailB()) end);
zip(Zip, {}, {HeadB, TailB}) ->
   new(HeadB, fun() -> zip(Zip, {}, TailB()) end);
zip(Zip, {HeadA, TailA}, {}) ->
   new(HeadA, fun() -> zip(Zip, TailA(), {}) end);
zip(_, {}, {}) ->
   {}.

%%
%% zip multiple streams
-spec(zip/2 :: (mapf(), [lazy()]) -> lazy()).

zip(Map, List) ->
   case [lazy:hd(X) || X <- List, X =/= {}] of
      []   -> {};
      Head -> new(Map(Head), fun() -> zip(Map, [lazy:tl(X) || X <- List, X =/= {}]) end)
   end.

%%
%% interleave streams
-spec(interleave/3 :: (zipf(), lazy(), lazy()) -> lazy()).

interleave(Zip, {HeadA, TailA}=A, {HeadB, TailB}=B) ->
   case Zip(HeadA, HeadB) of
      true  -> new(HeadA, fun() -> interleave(Zip, TailA(), B) end);
      false -> new(HeadB, fun() -> interleave(Zip, A, TailB()) end)
   end;
interleave(Zip, {}, {HeadB, TailB}) ->
   new(HeadB, fun() -> interleave(Zip, {}, TailB()) end);
interleave(Zip, {HeadA, TailA}, {}) ->
   new(HeadA, fun() -> interleave(Zip, TailA(), {}) end);
interleave(_, {}, {}) ->
   {}.


%%
%% build lazy stream from erlang data types
-spec(build/1 :: (any()) -> lazy()).

%%
%% build lazy stream from list
build([]) ->
   new();
build([H|T]) ->
   new(H, fun() -> build(T) end);

%%
%% build lazy stream from file description
build(FD)
 when is_tuple(FD), erlang:element(1, FD) =:= file_descriptor ->
   case file:read(FD, 8 * 1024) of
      {ok, Chunk} ->
         new(Chunk, fun() -> build(FD) end);
      eof  ->
         {}
   end.

%%
%% build a list for lazy stream
-spec(list/1 :: (lazy()) -> list()).
-spec(list/2 :: (integer(), lazy()) -> list()).


list(N, S) ->
   lists:reverse(
      lazy:hd(
         lazy:nth(N,
            lazy:fold(fun(X, Acc) -> [X | Acc] end, [], S)
         )
      )
   ).

%%
%%
list(S) ->
   lists:reverse(
      lazy:hd(
         lazy:mapfold(fun(eof, Acc) -> {Acc, []}; (H, Acc) -> [H | Acc] end, [], S)
      )
   ).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").


%% sequence of integers
integers(N) -> 
   lazy:advance(1, fun(X) when X < N -> X + 1; (_) -> eof end).

%% sequence of chars
chars(N) -> 
   lazy:advance($a, fun(X) when X < $a + N - 1 -> X + 1; (_) -> eof end).


lazy_stream_test() ->
   S   = lazy:new(1, fun() -> lazy:new(2) end),
   1   = lazy:hd(S),
   2   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   2   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).
 
lazy_advance_test() ->
   S  = integers(2),
   1  = lazy:hd(S),
   2  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1  = lazy:hd(lazy:nth(1, S)),
   2  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_filter_test() ->
   S = lazy:filter(fun(X) -> X rem 2 =:= 0 end, integers(5)),

   2   = lazy:hd(S),
   4   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   2   = lazy:hd(lazy:nth(1, S)),
   4   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_map_test() ->
   S = lazy:map(fun(X) -> X * X end, integers(2)),

   1   = lazy:hd(S),
   4   = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   1   = lazy:hd(lazy:nth(1, S)),
   4   = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_fold_1_test() ->
   S = lazy:fold(fun(X, Acc) -> Acc + X end, integers(3)),

   3  = lazy:hd(S),
   6  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   3  = lazy:hd(lazy:nth(1, S)),
   6  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).


lazy_fold_2_test() ->
   S = lazy:fold(fun(X, Acc) -> [X | Acc] end, [], integers(2)),

   [1]    = lazy:hd(S),
   [2, 1] = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   [1]    = lazy:hd(lazy:nth(1, S)),
   [2, 1] = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_unfold_1_test() ->
   S = lazy:unfold(fun(X, Acc) -> {[X, X - Acc], Acc} end, integers(3)),

   2  = lazy:hd(S),
   1  = lazy:hd(lazy:tl(S)),
   3  = lazy:hd(lazy:tl(lazy:tl(S))),
   2  = lazy:hd(lazy:tl(lazy:tl(lazy:tl(S)))),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(lazy:tl(lazy:tl(S)))))),

   2  = lazy:hd(lazy:nth(1, S)),
   1  = lazy:hd(lazy:nth(2, S)),
   3  = lazy:hd(lazy:nth(3, S)),
   2  = lazy:hd(lazy:nth(4, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(5, S))).

lazy_mapfold_1_test() ->
   S = lazy:mapfold(fun(X, Acc) -> {Acc + X, X} end, integers(3)),

   3  = lazy:hd(S),
   5  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   3  = lazy:hd(lazy:nth(1, S)),
   5  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_mapfold_2_test() ->
   S = lazy:mapfold(
      fun(X, Acc) -> 
         case X rem 2 of
            1 -> {lists:sum([X | Acc]), []};
            _ -> [X | Acc]
         end
      end,
      [],
      integers(5)
   ),

   1 = lazy:hd(S),  % [1]
   5 = lazy:hd(lazy:tl(S)), %[2, 3]
   9 = lazy:hd(lazy:tl(lazy:tl(S))), %[4, 5]
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(lazy:tl(S))))),

   1 = lazy:hd(lazy:nth(1, S)),
   5 = lazy:hd(lazy:nth(2, S)),
   9 = lazy:hd(lazy:nth(3, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(4, S))).

lazy_zip_1_test() ->
   S = lazy:zip(fun(A, B) -> {A, B} end, chars(2), integers(2)),

   {$a, 1}  = lazy:hd(S),
   {$b, 2}  = lazy:hd(lazy:tl(S)),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(S)))),

   {$a, 1}  = lazy:hd(lazy:nth(1, S)),
   {$b, 2}  = lazy:hd(lazy:nth(2, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(3, S))).

lazy_zip_2_test() ->
   S = lazy:zip(fun(A, B) -> {A, B} end, chars(3), integers(2)),

   {$a, 1}  = lazy:hd(S),
   {$b, 2}  = lazy:hd(lazy:tl(S)),
   $c       = lazy:hd(lazy:tl(lazy:tl(S))),
   {'EXIT', _} = (catch lazy:hd(lazy:tl(lazy:tl(lazy:tl(S))))),

   {$a, 1}  = lazy:hd(lazy:nth(1, S)),
   {$b, 2}  = lazy:hd(lazy:nth(2, S)),
   $c       = lazy:hd(lazy:nth(3, S)),
   {'EXIT', _} = (catch lazy:hd(lazy:nth(4, S))).

lazy_interleave_test() ->
   S = lazy:interleave(fun(A, B) -> A < B end, integers(2), integers(2)),
   [1, 1, 2, 2] = lazy:list(S).


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

lazy_list_test() ->
   [1, 2, 3, 4] = lazy:list(lazy:build([1, 2, 3, 4])).

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


