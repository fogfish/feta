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
%%   clone current node as slave
-module(clone). 

-export([
   start/1
  ,start/2
  ,start/3
  ,start_link/1
  ,start_link/2
  ,start_link/3
]).

%%
%% clone the node as slave
-spec(start/1 :: (atom()) -> {ok, node()} | {error, any()}).
-spec(start/2 :: (atom(), [atom()]) -> {ok, node()} | {error, any()}).
-spec(start/3 :: (atom(), [atom()], list()) -> {ok, node()} | {error, any()}).

start(Name) ->
   start(Name, [X || {X, _, _} <- application:which_applications()]).

start(Name, Apps) ->
   start(Name, Apps, []).

start(Name, Apps, Config) ->
   clone(start, Name, Apps, Config).

%%
%% clone the node as slave and link it to process
-spec(start_link/1 :: (atom()) -> {ok, node()} | {error, any()}).
-spec(start_link/2 :: (atom(), [atom()]) -> {ok, node()} | {error, any()}).
-spec(start_link/3 :: (atom(), [atom()], list()) -> {ok, node()} | {error, any()}).

start_link(Name) ->
   start_link(Name, [X || {X, _, _} <- application:which_applications()]).

start_link(Name, Apps) ->
   start_link(Name, Apps, []).

start_link(Name, Apps, Config) ->
   clone(start_link, Name, Apps, Config).

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
clone(Fun, Name, Apps, Config) ->
   case slave:Fun(host(), Name, args()) of
      {ok, Node} ->
         lists:foreach(
            fun(X) -> rpc:call(Node, code, add_pathz, [X]) end,
            code:get_path()
         ),
         lists:foreach(
            fun(X) -> rpc:call(Node, applib, boot, [X, Config]) end,
            Apps
         ),
         {ok, Node};
      Error ->
         Error
   end.

%%
%% return host name of current node
host() ->
   [_, Host] = binary:split(scalar:s(erlang:node()), <<$@>>),
   scalar:atom(Host).

%%
%% return command line arguments
args() ->
   Init = init:get_arguments(),
   lists:flatten([arg(X) || X <- Init] ++ cookie(Init) ++ sys()).

arg({Name, _})
 when Name =:= root orelse Name =:= progname orelse Name =:= home orelse 
      Name =:= name orelse Name =:= setcookie ->
   [];
arg({Name, Val}) ->
   Arg = [[$ , X, $ ] || X <- Val],
   [$ , $-, scalar:c(Name), Arg].

%%
%% return node cookie
cookie(Init) ->
   case lists:keyfind(setcookie, 1, Init) of
      {_, Cookie} ->
         [" -setcookie ", [[$ , X, $ ] || X <- Cookie]];
      false ->
         [" -setcookie ", scalar:c(erlang:get_cookie())]
   end.

%%
%% return system limits
sys() ->
   lists:map(
      fun(X) -> [$ , $+, X, $ , scalar:c(sys(X))] end,
      [$P, $K, $A]
   ).

sys($P) -> erlang:system_info(process_limit);
sys($K) -> erlang:system_info(kernel_poll);
sys($A) -> erlang:system_info(thread_pool_size).



