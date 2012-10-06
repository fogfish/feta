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
-export([whereis/2, members/1, size/1, address/2]).

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
join({hash, Addr}, Node, #ring{nodes=Nodes}=R) ->
   Npos = addr_to_int(Addr, R),
   List = case lists:keytake(Npos, 1, Nodes) of
      false         -> Nodes;
      {value, _, L} -> L
   end,
   R#ring{nodes=[{Npos, Node} | List]};

join(Key, Node, Ring) ->
   join(address(Key, Ring), Node, Ring).

%%
%% leave node
leave({hash, Addr}, #ring{nodes=Nodes}=R) ->
   Npos = addr_to_int(Addr, R),
   case lists:keytake(Npos, 1, Nodes) of
      false          -> R;
      {value, _, NN} -> R#ring{nodes=NN}
   end;

leave(Key, Ring) ->
   leave(address(Key, Ring), Ring).

%%
%% return list of nodes for the address
whereis({hash, Addr}, #ring{replica=N, nodes=Nodes}=R) ->
   Npos   = addr_to_int(Addr, R),
   {T, H} = lists:partition(
      fun({X, _}) -> X >= Npos end,
      lists:usort(Nodes)
   ),
   List = case length(T) of
      Len when Len >= N -> lists:sublist(T, N);
      Len               -> T ++ lists:sublist(H, N - Len)
   end,
   lists:map(fun({_, Peer}) -> Peer end, List);

whereis(Key, Ring) ->
   whereis(address(Key, Ring), Ring).

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
address({hash, Addr0}, #ring{keylen=Len}) ->
   <<Addr:Len/bits, _/bits>> = Addr0,
   {hash, Addr};

address(Val, #ring{hash=md5, keylen=Len}) ->
   <<Addr:Len/bits, _/bits>> = erlang:md5(term_to_binary(Val)),
   {hash, Addr};

address(Val, #ring{hash=sha1, keylen=Len})->
   <<Addr:Len/bits, _/bits>> = crypto:sha(term_to_binary(Val)),
   {hash, Addr}.




%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%%
addr_to_int(Addr, #ring{keylen=Len})
 when is_binary(Addr) ->
   <<Int:Len>> = Addr,
   Int.

% %%
% %%
% addr_to_int(Addr, #ring{keylen=Len})
%  when is_binary(Addr) ->
%    Size = erlang:min(bit_size(Addr), Len),
%    <<Int:Size, _/bits>> = Addr,
%    Int;

% addr_to_int(Addr, Ring) ->
%    addr_to_int(hash(Addr, Ring), Ring).
   
