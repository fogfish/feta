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
%%  @description
%%   in-memory hash tree (merkle tree):
%%     * each node holds particular range of key space. the root holds the 
%%       entire key space: [0, 2^160), root's i-th child [i * 2^160/64, (i + 1) * 2^160/64) and so on.
%%     * a leaf node contains keys to data elements
%%     * a leaf node is promoted to an internal node when its count exceeds 64. 
%%     * each successive 6 bits of the hash defines branch to traverse.
%%
%%  @todo
%%     * partial sync (peek subtree)
-module(ht).

-export([
   new/0
  ,insert/2
  ,insert/3
  ,lookup/2
  ,remove/2
  ,map/2
  ,foldl/3
  ,foldr/3
  ,list/1
  ,hash/1
  ,hash/2
  ,diff/2
  ,evict/2
]).

%%%------------------------------------------------------------------
%%%
%%% internal definition
%%%
%%%------------------------------------------------------------------

%% inner node
-record(ht, {
   type   = leaf,
   offset = 0,
   hash   = undefined,
   nodes  = []
}).


-type(hash()      :: binary()).
-type(key()       :: any()).
-type(tree()      :: {integer(), #ht{}}).
-type(signature() :: {hash, integer(), [hash()]}).

%% hash tree configuration
-define(HASH(X),  crypto:hash(sha, X)).
-define(BITS,     160).
-define(WIDTH,      6).
-define(CAPACITY,   64).

%% debug config
% -define(BITS,       8).
% -define(WIDTH,      1).
% -define(CAPACITY,   2).

%%
%% new hash tree
-spec(new/0 :: () -> tree()).

new() -> 
   {0, #ht{}}.
   
%%
%% insert a new entity to hash tree, any existed value is updated
-spec(insert/2 :: (key(), tree()) -> tree()).
-spec(insert/3 :: (hash(), key(), tree()) -> tree()).

insert(Key, Tree) ->
   insert(?HASH(Key), Key, Tree).
insert(Hash, Key, {N, #ht{}=Node}) ->
   {N + 1, ht_insert(1, Hash, Key, Node)}.

%%
%% recursive node insert
ht_insert(L, Hash, Key, #ht{type=leaf}=Node) ->
   % insert entity into leaf
   ht_split(
      L,
      Node#ht{
         % add an entity signature to leaf node hash 
         hash  = hadd(Hash, Node#ht.hash),
         nodes = ht_put_entity(Hash, Key, Node#ht.nodes)
      }
   );

ht_insert(L, Hash, Key, #ht{}=Node) ->
   % insert entity into inner node
   {N, NN} = ht_select_node(L, Hash, Node#ht.nodes),
   Node#ht{
      hash  = hadd(Hash, Node#ht.hash),
      nodes = [ht_insert(L + 1, Hash, Key, N) | NN]
   }.

%%
%% lookup entity, throws badarg exception if Hash is not present
-spec(lookup/2 :: (hash(), tree()) -> key()).

lookup(Hash, {_, #ht{}=Node}) ->
   ht_lookup(1, Hash, Node).

%%
%% lookup and accumulate a node that matches a key
ht_lookup(_, Hash, #ht{type=leaf}=Node) ->
   ht_get_entity(Hash, Node#ht.nodes);

ht_lookup(L, Hash, #ht{}=Node) ->
   ht_lookup(L + 1, Hash, ht_find_node(L, Hash, Node#ht.nodes)).

%%
%% remove entity from the tree
-spec(remove/2 :: (hash(), tree()) -> tree()).

remove(Hash, {N, #ht{}=Node}) ->
   {N - 1, ht_remove(1, Hash, Node)}.

%%
%% recursive key delete 
ht_remove(_, Hash, #ht{type=leaf}=Node) ->
   Node#ht{
      hash  = hsub(Hash, Node#ht.hash), 
      nodes = ht_remove_entity(Hash, Node#ht.nodes)
   };

ht_remove(L, Hash, #ht{}=Node) ->
   {N, NN} = ht_select_node(L, Hash, Node#ht.nodes),
   Node#ht{
      hash  = hsub(Hash, Node#ht.hash),
      nodes = [ht_remove(L + 1, Hash, N) | NN]
   }.

%%
%% map function over tree
-spec(map/2 :: (function(), tree()) -> tree()).

map(Fun, {_, #ht{}=Node}) ->
   map(Fun, Node);
map(Fun, #ht{type=leaf}=Node) ->
   lists:map(
      fun({_Hash, Key}) -> Fun(Key) end,
      Node#ht.nodes
   );
map(Fun, #ht{}=Node) ->
   lists:map(fun(X) -> map(Fun, X) end, Node#ht.nodes).


%%
%% fold function over tree
-spec(foldl/3 :: (function(), any(), tree()) -> any()).
-spec(foldr/3 :: (function(), any(), tree()) -> any()).

foldl(Fun, Acc, {_, #ht{}=Node}) ->
   foldl(Fun, Acc, Node);
foldl(Fun, Acc0, #ht{type=leaf}=Node) ->
   lists:foldl(
      fun({_Hash, Key}, Acc) -> Fun(Key, Acc) end,
      Acc0,
      Node#ht.nodes
   );
foldl(Fun, Acc0, #ht{}=Node) ->
   lists:foldl(fun(X, Acc) -> foldl(Fun, Acc, X) end, Acc0, Node#ht.nodes).

foldr(Fun, Acc, {_, #ht{}=Node}) ->
   foldr(Fun, Acc, Node);
foldr(Fun, Acc0, #ht{type=leaf}=Node) ->
   lists:foldr(
      fun({_Hash, Key}, Acc) -> Fun(Key, Acc) end,
      Acc0,
      Node#ht.nodes
   );
foldr(Fun, Acc0, #ht{}=Node) ->
   lists:foldr(fun(X, Acc) -> foldl(Fun, Acc, X) end, Acc0, Node#ht.nodes).

%%
%% return all elements from the tree
-spec(list/1 :: (tree()) -> [key()]).

list(T) ->
   foldl(fun(X, Acc) -> [X | Acc] end, [], T).   


%%
%% hash(Depth, HT) -> {hash, Depth, [Hash]}
%%
%% return list of signatures at depth (inner hashes only) or elements signature 
-spec(hash/1 :: (tree()) -> signature()).
-spec(hash/2 :: (integer(), tree()) -> signature()).

hash(L, {_, #ht{}=Node}) ->
   case ht_hash(L, [], Node) of
      [] -> throw(badarg); % signature is empty if depth exceed tree's depth
      H  -> {hash, L, H}
   end.

hash({_, #ht{}=Node}) ->
   {hash, 0, ht_hash(0, [], Node)}.

ht_hash(0, Acc0, {Hash, _Key}) ->
   [Hash | Acc0];
ht_hash(1, Acc0, #ht{}=Node) ->
   [Node#ht.hash | Acc0];   
ht_hash(_, Acc0, {_Hash, _Key}) ->
   Acc0;
ht_hash(L, Acc0, #ht{}=Node) ->
   lists:foldl(fun(X, Acc) -> ht_hash(dec(L), Acc, X) end, Acc0, Node#ht.nodes).

%%
%% calculates difference (reconciliation) of trees or signatures
-spec(diff/2 :: (signature() | tree(), signature() | tree()) -> signature() | {tree(), tree()}).

%% compares tree signatures, returns hash intersection
diff({hash, DA, SA}, {hash, DB, SB})
 when DA =:= DB ->
   L = lists:foldr(
      fun(X, Acc) -> 
         case lists:member(X, SA) of
            true  -> [X | Acc];
            false -> Acc
         end
      end,
      [],
      SB
   ),
   {hash, DA, L}; 
diff({hash, DA, _}, {hash, DB, _}) ->
   {hash, erlang:min(DA, DB), []};

%% performs a local ht reconciliation, return entity unique for each tree
diff({_, #ht{}}=A, {_, #ht{}}=B) ->
   ht_diff(1, A, B).

ht_diff(L, A, B) ->
   try
      I = ht:diff(ht:hash(L, A), ht:hash(L, B)),
      ht_diff(L + 1, ht:evict(I, A), ht:evict(I, B))
   catch
      throw:badarg -> 
         E = ht:diff(ht:hash(A), ht:hash(B)),
         {ht:evict(E, A), ht:evict(E, B)}
   end. 

%%
%% evict subtrees that matches a signature
-spec(evict/2 :: (signature(), tree()) -> tree()).

evict({hash, L, Hashes}, {N, #ht{}=Node}) ->
   {N, ht_evict(L, Hashes, Node)}.

ht_evict(0, Hashes, {Hash, _Key}=Node) ->
   case lists:member(Hash, Hashes) of
      true  -> undefined;
      false -> Node
   end;
ht_evict(1, Hashes, #ht{}=Node) ->
   case lists:member(Node#ht.hash, Hashes) of
      true  -> undefined;
      false -> Node
   end;
ht_evict(_, _Hashes, {_Hash, _Key}=Node) ->
   Node;
ht_evict(L,  Hashes, #ht{}=Node) ->
   Nodes = lists:foldl(
      fun(X, Acc) -> 
         case ht_evict(dec(L), Hashes, X) of
            undefined -> Acc;
            NNode     -> [NNode | Acc] 
         end
      end, 
      [], 
      Node#ht.nodes
      ),
   Node#ht{
      nodes = Nodes
   }.


   
%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

%%
%% hash add
hadd(undefined, Y) ->
   Y;
hadd(X, undefined) ->
   X;
hadd(X, Y) ->
   <<A:?BITS>> = X,
   <<B:?BITS>> = Y,
   <<(A bxor B):?BITS>>.

%%
%% hash subtract
hsub(X, Y) ->
   hadd(X, Y).

%%
%% decrement depth
dec(0) -> 0;
dec(N) -> N - 1. 


%%
%%
offset(L, Hash) ->
   % level numbering is starts from 1 ( 0 - is leaves, elements)
   Skip = (L - 1) * ?WIDTH,
   <<_:Skip, Val:?WIDTH, _/bitstring>> = Hash,
   Val. 

%%
%% put entity into leaf node container, 
%% if entity exists then it is updated
ht_put_entity(Hash, Key, Nodes) ->
   case lists:keytake(Hash, 1, Nodes) of
      {value, _, List} -> [{Hash, Key} | List];
      false            -> [{Hash, Key} | Nodes]
   end.

ht_get_entity(Hash, Nodes) ->
   case lists:keyfind(Hash, 1, Nodes) of
      {Hash, Key} -> Key;
      false       -> throw({bagrag, Hash})
   end.

ht_remove_entity(Hash, Nodes) ->
   case lists:keytake(Hash, 1, Nodes) of
      {value, _, List} -> List;
      false            -> throw({badarg, Hash})
   end.

%%
%% split inner node, child are divided into two nodes
%% the division chooses two most distant nodes and agregates 
%% rest of nodes around them.
ht_split(L, #ht{type=leaf, nodes=NN}=Node)
 when length(NN) > ?CAPACITY ->
   lists:foldl(
      fun({Hash, Key}, Acc) ->
         ht_insert(L, Hash, Key, Acc)
      end,
      #ht{type=inner, offset=Node#ht.offset},
      NN
   );
ht_split(_L, Node) ->
   Node.

     
%%
%% select and create node
ht_select_node(L, Hash, Nodes) ->
   I = offset(L, Hash),
   case lists:keytake(I, #ht.offset, Nodes) of
      {value, N, NN} -> 
         {N, NN};
      false          ->
         {#ht{offset=I}, Nodes}
   end.

ht_find_node(L, Hash, Nodes) ->
   I = offset(L, Hash),
   case lists:keyfind(I, #ht.offset, Nodes) of
      false   ->
         throw({badarg, Hash});
      #ht{}=N ->
         N
   end.





