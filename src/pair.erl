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
  ,x/2
  ,a/2
  ,rtop/2
  ,rtoa/2
  ,rtos/2
  ,ptor/3
  ,ator/3
  ,stor/3
]).

-type(key()   :: atom() | binary()).
-type(val()   :: any()).
-type(pairs() :: [{key(), val()}]).


%%
%% lookup value using path
%% throws exception if value do not exists
-spec lookup([key()], pairs()) -> val().

lookup([Key | Tail], Value) ->
   lookup(Tail, lookup_term(Key, Value));
lookup([], Value) ->
   Value;
lookup(Key, Value) ->
   lookup_term(Key, Value).   

%%
%% lookup value using path
%% return default value if path do not exists
-spec lookup([key()], val(), pairs()) -> val().

lookup(Path, Default, Pairs) ->
   try
      lookup(Path, Pairs)
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
   {Key, Val} = lists:keyfind(Key, 1, X),
   Val;
lookup_term(Key, X)
 when is_map(X) ->
   maps:get(Key, X).

%%
%% shortcut alias to lookup path
-spec x([key()], pairs()) -> val().

x(Path, Pairs) ->
   lookup(Path, undefined, Pairs).


%%
%% shortcut alias to check is path exists
-spec a([key()], pairs()) -> true | false.

a(Path, Pairs) ->
   X = lookup(Path, undefined, Pairs),
   X =/= undefined andalso X =/= false andalso X =/= [].


%%
%% record to pairs, build list of key / val pairs from record.
%% pair:rtol(record_info(fields, a), #a{}).
-spec rtop([atom()], tuple()) -> pairs().
-spec rtoa([atom()], tuple()) -> pairs().
-spec rtos([atom()], tuple()) -> pairs().

rtop(Struct, X) -> 
   rtoa(Struct, X).

rtoa(Struct, X)
 when is_tuple(X) ->
   lists:zip(Struct, tl(tuple_to_list(X))).

rtos(Struct, X)
 when is_tuple(X) ->
   lists:zip([scalar:s(Y) || Y <- Struct], tl(tuple_to_list(X))).


%%
%% pair to record, build record from pairs of record
%% pair:ptor(a, record_info(fields, a), [...]).
-spec ptor(atom(), [atom()], pairs()) -> tuple().
-spec stor(atom(), [atom()], pairs()) -> tuple().
-spec ator(atom(), [atom()], pairs()) -> tuple().

ptor(Type, Struct, X) ->
   ator(Type, Struct, X).

ator(Type, Struct, X) ->
   list_to_tuple([Type |  [lookup(Y, undefined, X) || Y <- Struct]]).

stor(Type, Struct, X) ->
   list_to_tuple([Type |  [lookup(scalar:s(Y),  X) || Y <- Struct]]).


