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
%%   pure functional queue
-module(q).
-define(TESTS, tests).

-export([
   new/0, new/1, hd/1, tl/1, enq/2, deq/1,
   is_empty/1, length/1, dropwhile/2
]).

-type(q() :: {q, integer(), list(), list()}).
-export_type([q/0]).

%%
%%
-spec(new/0 :: () -> q()).
-spec(new/1 :: (list()) -> q()).

new() ->
   {}.   

new([]) ->
    {};

new([_]=Head) ->
    {q, 1, [], Head};

new([X,Y]) ->
    {q, 2, [X],[Y]};

new([X,Y|Head]=List) ->
    {q, lists:length(List), [X,Y], lists:reverse(Head, [])}.

%%
%%
-spec(hd/1 :: (q()) -> any()).

hd({q, _Tail, [Head|_]}) ->
   Head;

hd({q, [Head | _], []}) ->
   Head;

hd(_) ->
   throw(badarg).

%%
%%
-spec(tl/1 :: (q()) -> q()).

tl(Q) ->
   {_, Tail} = deq(Q),
   Tail.

%%
%% enqueue element
-spec(enq/2 :: (any(), q()) -> q()).

enq(E, {q, N, [_]=Tail, []}) ->
   {q, N + 1, [E], Tail};

enq(E, {q, N, Tail, Head}) ->
   {q, N + 1, [E|Tail], Head};

enq(E, {}) ->
   {q, 1, [E], []}.

%%
%% dequeue element
-spec(deq/1 :: (q()) -> {any(), q()}).

deq({q, _, [E], []}) ->
   {E, q:new()};

deq({q, N, [Last|Tail], []}) ->
   [E|Head] = lists:reverse(Tail, []),
   {E, {q, N - 1, [Last], Head}};

deq({q, _, Tail, [E]}) ->
   {E, q:new(Tail)};

deq({q, N, Tail, [E|Head]}) ->
   {E, {q, N - 1, Tail, Head}}.

%%
%% check if the queue is empty
-spec(is_empty/1 :: (q()) -> boolean()).

is_empty({}) ->
   true;
is_empty(_) ->
   false.

%%
%%
length({q, N, _, _}) ->
   N;
length({}) ->
   0.

%%
%% dropwhile head of queue
-spec(dropwhile/2 :: (function(), q()) -> q()).

dropwhile(Pred, {q, _, _, _}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> dropwhile(Pred, Tail); 
      false -> Q
   end;

dropwhile(_,  {}) ->
   q:new().


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").

q_enc_deq_test() ->
   Seq = lists:seq(1, 5),
   Q0  = lists:foldl(fun q:enq/2, q:new(), Seq),
   {1, Q1} = q:deq(Q0),   
   {2, Q2} = q:deq(Q1),   
   {3, Q3} = q:deq(Q2),   
   {4, Q4} = q:deq(Q3),   
   {5, {}} = q:deq(Q4).   

q_hd_tl_test() ->
   Seq = lists:seq(1, 5),
   Q   = lists:foldl(fun q:enq/2, q:new(), Seq),
   1   = q:hd(Q),
   2   = q:hd(q:tl(Q)),
   3   = q:hd(q:tl(q:tl(Q))),
   4   = q:hd(q:tl(q:tl(q:tl(Q)))),
   5   = q:hd(q:tl(q:tl(q:tl(q:tl(Q))))),
   {}  = q:tl(q:tl(q:tl(q:tl(q:tl(Q))))).

q_dropwhile_test() ->
   Seq = lists:seq(1, 5),
   Q   = q:dropwhile(
      fun(X) -> X =< 2 end,
      lists:foldl(fun q:enq/2, q:new(), Seq)
   ),
   3   = q:hd(Q),
   4   = q:hd(q:tl(Q)),
   5   = q:hd(q:tl(q:tl(Q))),
   {}  = q:tl(q:tl(q:tl(Q))).

-endif.

