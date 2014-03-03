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
%%   consistent hashing - chord ring
-module(chord).

-export([
   new/0
  ,new/1
  ,size/1
  ,address/2
  ,whereis/2
  ,successors/2
  ,predecessors/2
  ,members/1
  ,shards/1
  ,member/2
  ,join/2
  ,join/3
  ,leave/2
]).

%%
-record(ring, {
   m      =   8       :: integer() % ring modulo
  ,n      =   3       :: integer() % number of replica   
  ,hash   = md5       :: atom()    % hash algorithm
  ,size   =   0       :: integer() % number of nodes
  ,shard  =   8       :: integer() % number of shards  
  ,shards = []        :: [{integer(), any()}]
}).

%%
%% create new chord ring
%%
%% Options:
%%   {modulo,  integer()}  - ring module power of 2 is required
%%   {hash,    md5 | sha1} - ring hashing algorithm
%%   {shard,   integer()}  - number of shard 
%%   {replica, integer()}  - number of replicas
-spec(new/0 :: () -> #ring{}).
-spec(new/1 :: (list()) -> #ring{}).

new() ->
   new([]).
new(Opts) ->
   init(Opts, #ring{}).

init([{modulo, X} | Opts], R) ->
   init(Opts, R#ring{m=X});
init([{replica, X} | Opts], R) ->
   init(Opts, R#ring{n=X});
init([{hash, X} | Opts], R) ->
   init(Opts, R#ring{hash=X});
init([{shard, X} | Opts], R) ->
   init(Opts, R#ring{shard=X});   
init([{_, _} | Opts], R) ->
   init(Opts, R);
init([], R) ->
   empty(R).

%%
%% number of nodes
-spec(size/1 :: (#ring{}) -> integer()).

size(#ring{}=R) ->
   R#ring.size.

%%
%% maps key into address on the ring
-spec(address/2 :: (any(), #ring{}) -> integer()).

address(X, #ring{}=R)
 when is_integer(X) ->
   X rem ringtop(R);

address({addr, X}, #ring{}=R) ->
   X rem ringtop(R);

address({hash, X}, #ring{m=M}) ->
   <<Addr:M, _/bits>> = X,
   Addr;

address(X, #ring{hash=Hash, m=M}) ->
   <<Addr:M, _/bits>> = crypto:hash(Hash, term_to_binary(X)),
   Addr.


%%
%% lookup shard and node pair at address
-spec(whereis/2 :: (integer(), #ring{}) -> {integer(), any()}).

whereis(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:dropwhile(fun({Id, _}) -> Id < Addr end, R#ring.shards) of
      []   -> hd(R#ring.shards);
      List -> hd(List)
   end;
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).

%%
%% return list of predecessors 
-spec(predecessors/2 :: (any(), #ring{}) -> [any()]).

predecessors(Addr, #ring{n=N}=R)
 when is_integer(Addr) ->
   {Head, Tail} = lists:partition(fun({Id, _}) -> Id < Addr end, R#ring.shards),
   case length(Head) of
      L when L >= N ->
         element(1, lists:split(N, lists:reverse(Head)));
      L ->
         lists:reverse(Head) ++ element(1, lists:split(N - L, lists:reverse(Tail)))
   end;
predecessors(Key, Ring) ->
   predecessors(address(Key, Ring), Ring).

%% 
%% return list of successors
-spec(successors/2 :: (any(), #ring{}) -> [any()]).

successors(Addr, #ring{n=N}=R)
 when is_integer(Addr) ->
   {Head, Tail} = lists:partition(fun({Id, _}) -> Id < Addr end, R#ring.shards),
   case length(Tail) of
      L when L >= N ->
         element(1, lists:split(N, Tail));
      L ->
         Tail ++ element(1, lists:split(N - L, Head))
   end;
successors(Key, Ring) ->
   successors(address(Key, Ring), Ring).

%%
%% return list of ring members
-spec(members/1 :: (#ring{}) -> [any()]).

members(#ring{}=S) ->
   lists:usort([Node || {_, Node} <- S#ring.shards]).

%%
%% test node membership
-spec(member/2 :: (any() | function(), #ring{}) -> false | {integer(), any()}).

member(Addr, #ring{}=R)
 when is_integer(Addr) ->
   lists:keyfind(Addr, 1, R#ring.shards);

member(Node, #ring{}=R) ->
   lists:keyfind(Node, 2, R#ring.shards).


%%
%% return list of shards 
-spec(shards/1 :: (#ring{}) -> [{integer(), any()}]).

shards(#ring{}=S) ->
   S#ring.shards.


%%
%% join node to the ring
-spec(join/2 :: (any(), #ring{}) -> #ring{}).
-spec(join/3 :: (any(), any(), #ring{}) -> #ring{}).

join(Node, Ring) ->
   join(Node, Node, Ring).

join(Addr, Node, #ring{}=R)
 when is_integer(Addr) ->
   R#ring{
      size   = R#ring.size + 1
     ,shards = orddict:store(Addr, Node, R#ring.shards)
   };

join(Addr, Node, Ring) ->
   join(address(Addr, Ring), Node, Ring).

%%
%% leave node from ring
-spec(leave/2 :: (any(), #ring{}) -> #ring{}).

leave(Addr, #ring{}=R)
 when is_integer(Addr) ->
   R#ring{
      size   = R#ring.size - 1
     ,shards = orddict:erase(Addr, R#ring.shards)
   };
leave(Node, #ring{}=R) ->
   Shards = orddict:filter(fun(_, N) -> N =/= Node end, R#ring.shards),
   R#ring{
      size   = orddict:size(Shards)
     ,shards = Shards 
   }.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% ring 
ringtop(#ring{}=R) ->
   trunc(math:pow(2, R#ring.m)).

%%
%% empties ring
empty(#ring{}=R) ->
   R#ring{
      size   = 0
     ,shards = orddict:new()
   }.




