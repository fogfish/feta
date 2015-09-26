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
-module(mime).

-export([new/1, get/2, match/2, to_binary/1]).

%%
%%
new(Str) when is_binary(Str) -> 
   {Type, SubType} = tokenize(Str),
   {mime, Type, SubType};

new(Str) when is_list(Str) ->
   new(list_to_binary(Str));

new(Str) when is_atom(Str) ->
   new(atom_to_binary(Str, utf8));

new({mime, _, _}=X) ->
   X.

%%
%%
get(type, {mime, V, _}) ->
   V;
get(subtype, {mime, _, V}) ->
   V;
get(Item, Type) 
 when is_binary(Type) orelse is_list(Type) -> 
   uri:get(Item, new(Type)).

%%
%%
match({mime, '*', _}, {mime, _, _}) ->
   true;
match({mime, _, _}, {mime, '*', _}) ->
   true;
match({mime, A, '*'}, {mime, B, _})
 when A =:= B ->
   true;
match({mime, A, _}, {mime, B, '*'})
 when A =:= B ->
   true;
match({mime, _, _}=TypeA, {mime, _, _}=TypeB)
 when TypeA =:= TypeB ->
   true;
match({mime, _, _}, {mime, _, _}) ->
   false;

match(Type, TType) ->
   match(new(Type), new(TType)).

%%
%%
to_binary({mime, Type, Subtype}) ->
   <<(atom_to_binary(Type, utf8))/binary, $/, (atom_to_binary(Subtype, utf8))/binary>>.


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

tokenize(Str) ->
   % TODO: support q-values
   Str1 = case binary:split(Str, <<$;>>, []) of
      [Type]    -> Type;
      [Type, _] -> Type
   end,
   case binary:split(Str1, <<$/>>, []) of
      [Type1, Subtype] -> 
         {binary_to_atom(Type1, utf8), binary_to_atom(Subtype, utf8)};
      [Type1] ->
         {binary_to_atom(Type1, utf8), '*'}
   end.