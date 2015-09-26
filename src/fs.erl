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
%%   file system utility
-module(fs).

-export([
   expand/1,
   foreach/2,
   fold/3
]).

-type(path() :: list()).

%%
%% expand path with node name
expand([Path | Tail])
 when is_list(Path) orelse is_binary(Path) ->
   [Node, _] = string:tokens(scalar:c(erlang:node()), "@"),
   filename:join([scalar:c(Path), Node | Tail]);
expand(Path) ->
   [Node, _] = string:tokens(scalar:c(erlang:node()), "@"),
   filename:join([scalar:c(Path), Node]).


%%
%% applies a function to each file for its side-effects;
%% it returns nothing.
-spec(foreach/2 :: (function(), path()) -> ok).

foreach(Fun, Path) ->
   case filelib:is_dir(Path) of
      true  ->
         case file:list_dir(Path) of
            {ok, List} ->
               lists:foreach(
                  fun(X) ->
                     foreach(Fun, X)
                  end,
                  [filename:join(Path, X) || X <- List]
               );
            {error, _Reason} ->
               ok
         end;
      false -> 
         Fun(Path)
   end.

%%
%%
-spec(fold/3 :: (function(), any(), path()) -> ok).

fold(Fun, Acc, Path) ->
   case filelib:is_dir(Path) of
      true  ->
         case file:list_dir(Path) of
            {ok, List} ->
               lists:foldl(
                  fun(X, Acc0) ->
                     fold(Fun, Acc0, X)
                  end,
                  Acc,
                  [filename:join(Path, X) || X <- List]
               );
            {error, _Reason} ->
               Acc
         end;
      false -> 
         case filelib:is_file(Path) of
            true ->
               Fun(Path, Acc);
            false->
               Acc
         end
   end.
