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
%%   lazy evaluation streams http://www.serenethinking.com/2011/10/lazy-evaluation-and-streams-in-erlang/
-module(streams).

-export([
   new/0, new/1, new/2, seq/2, hd/1, tl/1,
   filter/2, map/2, fold/2, reduce/2
]).
-type(streams()   :: {any(), function()} | {}).

%%
%% create new streams
-spec(new/0 :: () -> streams()).
-spec(new/1 :: (any()) -> streams()).
-spec(new/2 :: (any(), function()) -> streams()).

new() ->
  {}.

new(Head) ->
  {Head, fun() -> {} end}.

new(Head, Fun) ->
   {Head, Fun}.

%%
%% create new stream, next element is mapped by function from the previous one.
-spec(seq/2 :: (any(), function()) -> streams()).

seq(Head, Fun) ->
   new(Head, fun() -> seq(Fun(Head), Fun) end).

%%
%% head element
-spec(hd/1 :: (streams()) -> any()).

hd({Head, _}) ->
   Head.

%%
%% tail stream
-spec(tl/1 :: (streams()) -> streams()).

tl({_, Fun}) ->
   Fun().

%%
%% return a new stream with filtered values.
-spec(filter/2 :: (function(), streams()) -> streams()).

filter(Pred, {Head, Tail}) ->
   case Pred(Head) of
      true ->
         new(Head, fun() -> filter(Pred, Tail()) end);
      false ->
         filter(Pred, Tail())
   end.

%%
%% returns a new stream with mapped values.
-spec(map/2 :: (function(), streams()) -> streams()).

map(Fun, {Head, Tail}) ->
   new(Fun(Head), fun() -> map(Fun, Tail()) end).


%%
%% return a new stream with folded elements
-spec(fold/2 :: (function(), streams()) -> streams()).

fold(Pred, Stream) ->
  fold(Pred, Stream, []).

fold(Pred, {Head, Tail}, Acc) ->
  case Pred(Head) of
    false ->
      new(lists:reverse([Head | Acc]), fun() -> fold(Pred, Tail()) end);
    true  ->
      fold(Pred, Tail(), [Head | Acc])
  end.


% Reduces N elements of a stream to a single value by applying reduce function
% ReduceFun to the first N elements.
reduce(Fun, {Head, Tail}) ->
   reduce(Fun, Head, Tail()).

reduce(Fun, Acc0, {Head, Tail}) ->
   case Fun(Head, Acc0) of
      {false, Acc} ->
         reduce(Fun, Acc, Tail());
      {true,  Val} ->
         new(Val, fun() -> reduce(Fun, Tail()) end)
   end.
