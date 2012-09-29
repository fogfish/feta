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
%%   consistent hash
-module(ring).

-export([new/0, new/1, join/3, leave/2]).
-export([whereis/2, members/1, size/1]).
-export([hash/2]).

%%
-record(ring, {
   keylen = 128,    % length of key
   hash   = md5,    % hashing
   replica=   1,    % number of replica
   nodes  =  []     % nodes
}).


%%
%% create new ring
new() ->
   #ring{}.
new(Opts) ->
   new(Opts, #ring{}).

new([{hash, X} | Opts], R) ->
   new(Opts, R#ring{hash=X});
new([{keylen, X} | Opts], R) ->
   new(Opts, R#ring{keylen=X});
new([{replica, X} | Opts], R) ->
   new(Opts, R#ring{replica=X});
new([], R) ->
   R.

%%
%% join node to the ring
join(Key, Node, #ring{nodes=Nodes}=R) ->
   Addr = addr_to_int(Key, R),
   List = case lists:keytake(Addr, 1, Nodes) of
      false         -> Nodes;
      {value, _, L} -> L
   end,
   R#ring{nodes=[{Addr, Node} | List]}.

%%
%% leave node
leave(Key, #ring{nodes=Nodes}=R) ->
   Addr = addr_to_int(Key, R),
   case lists:keytake(Addr, 1, Nodes) of
      false          -> R;
      {value, _, NN} -> R#ring{nodes=NN}
   end.

%%
%% return list of nodes for the key
whereis(Key, #ring{replica=N, nodes=Nodes}=R) ->
   Addr   = addr_to_int(Key, R),
   {T, H} = lists:partition(
      fun({X, _}) -> X >= Addr end,
      lists:usort(Nodes)
   ),
   List = case length(T) of
      Len when Len >= N -> lists:sublist(T, N);
      Len -> T ++ lists:sublist(H, N - Len)
   end,
   lists:map(fun({_, Peer}) -> Peer end, List).

%%
%% return list of ring members
members(#ring{nodes=Nodes}) ->
   lists:map(fun({_, Peer}) -> Peer end, Nodes).

%%
%% number of members
size(#ring{nodes=Nodes}) ->
   length(Nodes).

%%
%%
hash(Val, #ring{hash=md5}) ->
   erlang:md5(term_to_binary(Val));

hash(Val, #ring{hash=sha1})->
   crypto:sha1(term_to_binary(Val)).

%%
%%
addr_to_int(Addr, #ring{keylen=Len})
 when is_binary(Addr) ->
   Size = erlang:min(bit_size(Addr), Len),
   <<Int:Size, _/bits>> = Addr,
   Int;

addr_to_int(Addr, Ring) ->
   addr_to_int(hash(Addr, Ring), Ring).
   
