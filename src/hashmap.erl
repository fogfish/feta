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
%%    hashmap
-module(hashmap).

-export([new/0, new/1, size/1]).
-export([put/3, get/2, remove/2]).
-export([foldl/3, foldr/3, elements/1]).
-export([fchunk/3, drop/2]).


%% internal structure
-record(hmap, {
   chnk = 256,   % capacity of node
   bits = 8,     % width of node offset
   root          % root node
}).

%%
%%
-record(chnk, {
   offset= nil   :: integer(),    % current node offset
   depth = 1     :: integer(),    % node depth (1 is leaf)
   count = 0     :: integer(),    % number elements in subtrees
   inner = []    :: list()        % inner elements, key/val tuples
}).


%%%------------------------------------------------------------------
%%%
%%% API 
%%%
%%%------------------------------------------------------------------

%%
%% new(Opts) -> Hashmap
%%   Opts = [Opts]
%% 
new() -> 
   new([]).

new(Opts) ->
   new(Opts, #hmap{root=#chnk{}}).

new([{bits, X} | Opts], T) ->
   new(Opts, T#hmap{bits=X, chnk=1 bsl X});

new([], T) ->
   T.

%%
%% number of element
size(#hmap{root=#chnk{count=Size}}) ->
   Size.


%%
%% put(Entity, HT) -> NewHT
%% put(Key, Entity, HT) -> NewHT
%%   Entity = term()
%%   Key = binary()
%%
%% insert a new entity to hash tree, any existed value is updated
put(Key, El, #hmap{root=Root}=T) ->
   T#hmap{
      root = ht_put(0, Key, El, Root, T)
   }.

%% recursive put element into tree
ht_put(_, Key, El, nil, _Opts) ->
   {Key, El};

ht_put(D, Key, El, Node0, #hmap{bits=Bits}=Opts) ->
   Offs = offset(Key, Bits, D),
   {Sub, Node} = case take_node(Key, Offs, Node0) of 
      {{Key,_}, NN} -> {nil, NN};
      {nil,     NN} -> {#chnk{offset=Offs}, NN};
      {N,       NN} -> {N, NN}
   end,
   split(
      push_node(
         ht_put(D + 1, Key, El, Sub, Opts),
         Node
      ),
      D,
      Opts
   ).

%%
%% get(Key, HT) -> Entity | false
%%
%% get entity
get(Key, #hmap{root=Root} = T) ->
   ht_get(0, Key, Root, T).

% recursive get
ht_get(D, Key, Node, #hmap{bits=Bits}=Opts) ->
   Offs = offset(Key, Bits, D),
   case take_node(Key, Offs, Node) of
      {{Key, nil}, _} -> false;
      {{Key,   E}, _} -> E;
      {nil,        _} -> false;
      {Sub,        _} -> ht_get(D + 1, Key, Sub, Opts)
   end.

%%
%% remove(Key, HT) -> NewHT
%%
%% removes entity from the tree
remove(Key, #hmap{root=Root}=T) ->
   T#hmap{
      root = ht_remove(0, Key, Root, T)
   }.

%% recursive node remove
ht_remove(_D, _El, nil, _Opts) ->
   nil;

ht_remove(D, Key, Node0, #hmap{bits=Bits}=Opts) ->
   Offs = offset(Key, Bits, D),
   {Sub, Node} = case take_node(Key, Offs, Node0) of 
      {{Key, _}, NN} -> {nil, NN};
      {nil,      NN} -> {nil, NN};
      {N,        NN} -> {N,   NN}
   end,
   push_node(
      ht_remove(D + 1, Key, Sub, Opts),
      Node
   ).

%%
%%
foldl(Fun, Acc, #hmap{root=Root}) ->
   ht_foldl(Fun, Acc, Root).

ht_foldl(Fun, Acc0, #chnk{depth=1, inner=Inn}) ->
   lists:foldl(
      fun({_Key, El}, Acc) -> Fun(El, Acc) end,
      Acc0,
      Inn
   );

ht_foldl(Fun, Acc0, #chnk{inner=Inn}) ->
   lists:foldl(
      fun(X, Acc) -> ht_foldl(Fun, Acc, X) end,
      Acc0,
      Inn 
   ).

%%
%%
foldr(Fun, Acc, #hmap{root=Root}) ->
   ht_foldr(Fun, Acc, Root).

ht_foldr(Fun, Acc0, #chnk{depth=1, inner=Inn}) ->
   lists:foldr(
      fun({_Key, El}, Acc) -> Fun(El, Acc) end,
      Acc0,
      Inn
   );

ht_foldr(Fun, Acc0, #chnk{inner=Inn}) ->
   lists:foldr(
      fun(X, Acc) -> ht_foldl(Fun, Acc, X) end,
      Acc0,
      Inn 
   ).

%%
%% elements(HT) -> [Entity]
%%
%% return all elements from the tree
elements(Map) ->
   foldl(fun(E, A) -> [E|A] end, [], Map).


%%
%% fold function over chunks
fchunk(Fun, Acc, #hmap{bits=Bits, root=Root}) ->
   ht_fchunk(<<>>, Fun, Acc, Root, Bits).

ht_fchunk(Pfx, Fun, Acc0, #chnk{depth=1, offset=Offs, inner=Inn}, Bits) ->
   Fun({<<Pfx/bits, Offs:Bits>>, Inn}, Acc0);

ht_fchunk(_Pfx, Fun, Acc0, #chnk{offset=nil, inner=Inn}, Bits) ->
   lists:foldl(
      fun(X, Acc) -> 
         ht_fchunk(<<>>, Fun, Acc, X, Bits) 
      end,
      Acc0,
      Inn 
   );

ht_fchunk(Pfx, Fun, Acc0, #chnk{offset=Offs, inner=Inn}, Bits) ->
   lists:foldl(
      fun(X, Acc) -> 
         ht_fchunk(<<Pfx/bits, Offs:Bits>>, Fun, Acc, X, Bits) 
      end,
      Acc0,
      Inn 
   ).

%%
%% drop chunk
drop(Key, #hmap{root=Root}=T) ->
   T#hmap{
      root = ht_drop(0, Key, Root, T)
   }.

%% recursive node remove
ht_drop(D, Key, _Node, #hmap{bits=Bits})
  when Bits * D =:= bit_size(Key) ->
   nil;

ht_drop(D, Key, Node0, #hmap{bits=Bits}=Opts) ->
   Offs = offset(Key, Bits, D),
   {Sub, Node} = case take_node(Key, Offs, Node0) of 
      {{Key, _}, NN} -> {nil, NN};
      {nil,      NN} -> {nil, NN};
      {N,        NN} -> {N,   NN}
   end,
   push_node(
      ht_drop(D + 1, Key, Sub, Opts),
      Node
   ).

%%
%%
%subset(Key, #hmap{root=Root}=T) ->
%    case ht_subset(0, Key, Root, T) of
%       false -> false;
%       Node  -> T#hmap{root=Node}
%    end.


% %% recursive subset
% ht_subset(D, Key,  Node, #hmap{bits=Bits})
%  when Bits * D =:= bit_size(Key) ->
%    Node;

% ht_subset(D, Key, Node, #hmap{bits=Bits}=Opts) ->
%    Offs = offset(Key, Bits, D),
%    case take_node(Key, Offs, Node) of
%       {{Key, _}, _} -> false;
%       {{Key, E}, _} -> false;
%       {nil,      _} -> false;
%       {Sub,      _} -> ht_subset(D + 1, Key, Sub, Opts)
%    end.




%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

%%
%% calculate offset
offset(Key, Bits, Depth) ->
   Skip = Depth * Bits,
   <<_:Skip, Offs:Bits, _/bits>> = Key,
   Offs.

%%
%% take node from node set
take_node(Key, _Offs, #chnk{depth=1, count=Cnt, inner=Inn}=N) ->
   case lists:keytake(Key, 1, Inn) of
      false            -> {{Key, nil}, N};
      {value, E, NInn} -> {E, N#chnk{count=Cnt, inner=NInn}}
   end;

take_node(_Key, Offs, #chnk{count=Cnt, inner=Inn}=N) ->
   case lists:keytake(Offs, 2, Inn) of
      false            -> {nil,    N};
      {value, E, NInn} -> {E, N#chnk{count=Cnt, inner=NInn}}
   end.

%%
%% updated node
push_node({_Key, _}=El, #chnk{count=Cnt, inner=Inn}=Node) ->
   Node#chnk{
      count = Cnt + 1,
      inner = [El | Inn]
   };

push_node(#chnk{offset=_Offs, depth=Cd}=El, #chnk{depth=Pd, count=Cnt, inner=Inn}=Node) ->
   Node#chnk{
      depth = max(Pd, Cd + 1),
      count = Cnt + 1,
      inner = [El | Inn]
   };

push_node(nil, Node) ->
   Node.


%%
%% split node
split(#chnk{depth=1, inner=Inn}=Node, _D, #hmap{chnk=C})
 when length(Inn) =< C ->
   Node;

split(#chnk{offset=Offs, depth=1, count=Cnt, inner=Inn}, D, #hmap{chnk=C}=Opts)
 when length(Inn) > C ->
   lists:foldl(
      fun({Key, El}, Acc) -> ht_put(D, Key, El, Acc, Opts) end,
      #chnk{offset=Offs, count=Cnt, depth = 2},
      Inn
   );

split(#chnk{inner=Inn}=Node, _, #hmap{chnk=C})
 when length(Inn) =< C ->
   Node.


%%
%% calls Fun(Node, Acc) on Nodes of Tree, 
%% starting with AccIn == Acc0. 
%% Fun/2 must return a new accumulator which is passed to the next call. 
%% The function returns the final value of the accumulator. 
%% Acc0 is returned if the tree is empty.
% fold(Fun, Acc0, #chnk{inner=Inn}=Node) ->
%    case Fun(Node, Acc0) of
%       skip        -> Acc0;
%       {skip, Acc} -> Acc;
%       Acc         ->
%          lists:foldl(
%             fun
%                ({_, #chnk{}=Sub}, A) -> fold(Fun, A, Sub); 
%                (Entity, A) -> Fun(Entity, A)
%             end,
%             Acc,
%             Inn
%          )
%    end.

