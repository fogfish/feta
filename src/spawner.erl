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
%%   utility to spawn erlang node on local host
-module(spawner).

-export([
   start/3
  ,new/1
  ,free/1
  ,run/1
]).

%%
%% compatibility wrapper for slave
-spec(start/3 :: (atom(), atom(), list()) -> {ok, node()} | {error, any()}).

start(_Host, Node, _Opts) ->
   new(Node).

%%
%% spawn new node
-spec(new/1 :: (atom()) -> {ok, node()} | {error, any()}).

new(Name) ->
   Node = scalar:atom(host(Name)),
   case lists:member(Node, erlang:nodes()) of
      true  ->
         {ok, Node};

      false ->
         net_kernel:monitor_nodes(true),
         _ = open_port({spawn, erl(Name)}, [stream]),
         receive
            {nodeup, Node} ->
               net_kernel:monitor_nodes(false),
               {ok, Node}
         after 10000 ->
               net_kernel:monitor_nodes(false),
               {error, timeout}
         end
   end.

%%
%% terminate node
-spec(free/1 :: (atom()) -> ok).

free(Name) ->
   Node = scalar:atom(host(Name)),    
   case lists:member(Node, erlang:nodes()) of
      true  ->
         rpc:call(Node, erlang, halt, []), ok; 
      false ->
         ok
   end.

%%
%% run management process
run([Host]) ->
   spawn(
      fun() ->
         try
            Node = scalar:atom(Host),
            true = net_kernel:connect_node(Node),
            erlang:monitor_node(Node, true),
            run(slave, Node)
         catch _:Reason ->
            error_logger:error_report([{fatal, Reason}]),
            halt()
         end
      end
   ).

run(slave, Node) ->
   receive
      {nodedown, Node} ->
         halt();
      _ ->
         run(slave, Node)
   end.


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% return host name of current node
host(Node) ->
   [_, Host] = binary:split(scalar:s(erlang:node()), <<$@>>),
   scalar:c(Node) ++ "@" ++ scalar:c(Host).

%%
%% return command line arguments
erl(Node) ->
   Init = init:get_arguments(),
   Base = "erl -detached -noshell -noinput ",
   Name = " -name " ++ host(Node),
   Kick = " -run spawner run " ++ scalar:c(erlang:node()),
   lists:flatten(Base ++ Name ++ [arg(X) || X <- Init] ++ cookie(Init) ++ sys() ++ Kick).

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



