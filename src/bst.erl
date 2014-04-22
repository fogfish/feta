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
%%  @description
%%   binary search tree
-module(bst).

-export([
   new/0         %% O(1)
  ,new/1         %% O(n)
  ,insert/3      %% O(log n)
  ,lookup/2      %% O(log n)
  ,head/1        %% O(log n)
  ,map/2         %% O(n)
  ,foldl/3       %% O(n)
  ,foldr/3       %% O(n)
  ,splitwith/2   %% O(log n)
  ,takewhile/2   %% O(log n)
  ,take/2        %% O(log n)
  ,dropwhile/2   %% O(log n)
  ,drop/2        %% O(log n)  
]).
-type_export([tree/0]).

-define(NULL,   nil).

-type(tree() :: {tree(), key(), val(), tree()} | ?NULL).
-type(key()  :: any()).
-type(val()  :: any()).

%%
%% create new binary search tree
-spec(new/0 :: () -> tree()).
-spec(new/1 :: (list()) -> tree()).

new()  ->
   ?NULL.

new([]) ->
   ?NULL;
new([{K, V}]) ->
   {?NULL, K, V, ?NULL};
new([K])  ->
   {?NULL, K, undefined, ?NULL};
new(List) ->
   case lists:split(length(List) div 2, List) of
      {L, [{K, V} | R]} ->
         {new(L), K, V, new(R)};
      {L, [K | R]} ->
         {new(L), K, undefined, new(R)}
   end.

%%
%% insert element
-spec(insert/3 :: (key(), val(), tree()) -> tree()).

insert(K, V, ?NULL) ->
   {?NULL, K, V, ?NULL};
insert(K, V, {A, Kx, _, B})
 when K =:= Kx ->
   {A, Kx, V, B};
insert(K, V, {A, Kx, Vx, B})
 when K  >  Kx ->
   {A, Kx, Vx, insert(K, V, B)};
insert(K, V, {A, Kx, Vx, B})
 when K  <  Kx ->
   {insert(K, V, A), Kx, Vx, B}.

%%
%% lookup element
-spec(lookup/2 :: (key(), tree()) -> val()).

lookup(_, ?NULL) ->
   undefined;
lookup(K, {_, Kx, Vx, _})
 when K =:= Kx ->
   Vx;
lookup(K, {_, Kx,  _, B})
 when K  >  Kx ->
   lookup(K, B);
lookup(K, {A, Kx,  _, _})
 when K  <  Kx ->
   lookup(K, A).

%%
%% return first element
-spec(head/1 :: (tree()) -> {key(), val()}).

head({?NULL, K, V, _}) ->
   {K, V};
head({A, _, _, _}) ->
   head(A).

%%
%% map tree
-spec(map/2 :: (function(), tree()) -> tree()).

map(_Fun, ?NULL) ->
   ?NULL;
map(Fun, {A, K, V, B}) ->
   {Kx, Vx} = Fun(K, V),
   {map(Fun, A), Kx, Vx, map(Fun, B)}.

%%
%% fold function over tree 
-spec(foldl/3 :: (function(), any(), tree()) -> any()).
-spec(foldr/3 :: (function(), any(), tree()) -> any()).

foldl(_Fun, Acc0, ?NULL) ->
   Acc0;
foldl(Fun, Acc0, {A, K, V, B}) ->
   foldl(Fun, Fun(K, V, foldl(Fun, Acc0, A)), B).

foldr(_Fun, Acc0, ?NULL) ->
   Acc0;
foldr(Fun, Acc0, {A, K, V, B}) ->
   foldr(Fun, Fun(K, V, foldr(Fun, Acc0, B)), A).

%%
%% split
-spec(splitwith/2 :: (function(), tree()) -> {tree(), tree()}).

splitwith(_Fun, ?NULL) ->
   {?NULL, ?NULL};

splitwith(Fun, {A, K, V, B}) ->
   case Fun(K, V) of
      false ->
         {Ax, Bx} = splitwith(Fun, A),
         {Ax, {Bx, K, V, B}};
      true  ->
         {Ax, Bx} = splitwith(Fun, B),
         {{A, K, V, Ax}, Bx}
   end.

%%
%%
-spec(takewhile/2 :: (function(), tree()) -> tree()).

takewhile(_Fun, ?NULL) ->
   ?NULL;

takewhile(Fun, {A, K, V, B}) ->
   case Fun(K, V) of
      false ->
         takewhile(Fun, A);
      true  ->
         {A, K, V, takewhile(Fun, B)}
   end.

%%
%%
-spec(take/2 :: (integer(), tree()) -> tree()).

take(N, T) ->
   erlang:element(2, take1(N, T)).

take1(N, ?NULL) ->
   {N, ?NULL};
take1(N, {A, K, V, B}) ->
   case take1(N, A) of
      {0, Ax} ->
         {0, Ax};
      {M, Ax} ->
         {R, Bx} = take1(M - 1, B),
         {R, {Ax, K, V, Bx}}
   end.

%%
%%
-spec(dropwhile/2 :: (function(), tree()) -> tree()).

dropwhile(_Fun, ?NULL) ->
   ?NULL;

dropwhile(Fun, {A, K, V, B}) ->
   case Fun(K, V) of
      false ->
         {dropwhile(Fun, A), K, V, B};
      true  ->
         dropwhile(Fun, B)
   end.

%%
%%
-spec(drop/2 :: (integer(), tree()) -> tree()).

drop(N, T) ->
   erlang:element(2, drop1(N, T)).

drop1(N, ?NULL) ->
   {N, ?NULL};
drop1(N, {A, K, V, B}) ->
   case drop1(N, A) of
      {0, Ax} ->
         {0, {Ax, K, V, B}};
      {M, Ax} ->
         drop1(M - 1, B)
   end.




% iterator({nil, _, _, _} = T, As) ->
%     [T | As];
% iterator({L, _, _, _} = T, As) ->
%     iterator(L, [T | As]);
% iterator(nil, As) ->
%     As.

% next([{X, V, _, T} | As]) ->
%     {X, V, iterator(T, As)};
% next([]) ->
%     none.
