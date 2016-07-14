%%
%%   Copyright 2012 - 2014 Dmitry Kolesnikov, All Rights Reserved
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
%%   knowledge triple encode / decode (simples triple exchange format based on csv)
-module(knowledge).

-export([
   stream/1
]).

%%
%% stream decoder
-spec stream(stdio:stream()) -> stdio:stream().

stream(Stream) ->
   entity( knowledge( csv:stream(Stream) ) ).

knowledge({s, _, _} = Stream) ->
   case stdio:head(Stream) of
      L when length(L) =:= 3 ->
         stdio:new(decode(L), fun() -> knowledge(stdio:tail(Stream)) end);
      _ ->
         knowledge(stdio:tail(Stream))
   end;
knowledge({}) ->
   stdio:new().
   
%%
%% decode external data
decode([Uid, Pred, <<$", _/binary>>=Lit]) ->
   Val = binary:part(Lit, 1, byte_size(Lit) - 2),
   [{id, Uid}, {decode(Pred), Val}];

decode([Uid, Pred, Lit]) ->
   Val = scalar:decode(Lit),
   [{id, Uid}, {decode(Pred), Val}];

decode(<<$:, Pred/binary>>) ->
   scalar:atom(Pred);

decode(Pred) ->
   Pred.


%%
%% produces stream of entity
%% fold stream by entity id
-spec entity(datum:stream()) -> datum:stream().

entity({s, [{id, Id} | _], _}=Stream) ->
   reduce(Id, [], Stream);

entity({}) ->
   stdio:new().

reduce(Id, Acc, {s, _, _} = Stream) ->
   case stream:head(Stream) of
      [{id, Id}, X] ->
         reduce(Id, [X|Acc], stream:tail(Stream));
      _ ->
         stdio:new([{id, Id}|lists:reverse(Acc)], fun() -> entity(Stream) end)
   end;

reduce(Id, Acc, {}) ->
   stdio:new([{id, Id}|lists:reverse(Acc)]).


