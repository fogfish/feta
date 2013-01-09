%% @description
%%    Ring is a consistent hashing schema on ring modulo 2^m
%%    The key space is divided into equally sized shards.
%%    Shards are claimed and release by nodes. There are two 
%%    allocation strategy   
%%   
%%    Strategy 1 (chord):
%%    This is classical chord ring. Node address 
%%    is derived from it's identifier. Node controls all
%%    complete shards clockwise from its address (successor
%%    shards)
%%     
%%    Strategy 2 (token):    
%%    Each node claims claims S/N shards (S number of shards, 
%%    N number of nodes). 
%%
-module(ring2).

-export([new/1, new/2, join/2, join/3, leave/2]).
-export([address/2, members/1, is_member/2, shards/1, shards/2, whereis/2]).
-export([successors/2, predecessors/2]).

%%
-record(ring, {
   type   = chord,  % shard allocation strategy
   m      =   8,    % ring modulo
   n      =   3,    % number of replica   
   hash   = md5,    % hash algorithm
   shard  =   8,    % number of shards
   node   =   0,    % number of nodes
   shards =  []     % list of shards
}).

%%
%% create new ring and seed it with node
new(Seed) ->
   new(Seed, []).
new(Seed, Opts) ->
   init(Opts, #ring{node=Seed}).

init([{type, X} | Opts], R) ->
   init(Opts, R#ring{type=X});

init([{modulo, X} | Opts], R) ->
   init(Opts, R#ring{m=X});

init([{replica, X} | Opts], R) ->
   init(Opts, R#ring{n=X});

init([{hash, X} | Opts], R) ->
   init(Opts, R#ring{hash=X});

init([{shard, X} | Opts], R) ->
   % TODO: validate shard is modulo 2
   init(Opts, R#ring{shard=X});

init([{_, _} | Opts], R) ->
   init(Opts, R);

init([], #ring{m=M, shard=Q, node=Seed}=R) ->
   Top = trunc(math:pow(2,M)),
   Inc = Top div Q,
   R#ring{
      node   = 1,
      shards = [{X, Seed} || X <- lists:seq(Inc, Top, Inc)]
   }.

%%
%% address(Key, Ring) -> Addr
%%
%% maps key into address on the ring
address({hash, X}, #ring{m=M}) ->
   <<Addr:M, _/bits>> = X,
   Addr;

address(X, #ring{hash=md5, m=M}) ->
   <<Addr:M, _/bits>> = erlang:md5(term_to_binary(X)),
   Addr;

address(X, #ring{hash=sha1, m=M})->
   <<Addr:M, _/bits>> = crypto:sha(term_to_binary(X)),
   Addr.

%%
%% members(Ring) -> Nodes
%%
%% return list of ring members
members(#ring{shards=Shards}) ->
   lists:usort([X || {_, X} <- Shards]).

%%
%% is_member(Node, Ring) -> true | false
%%
%% check if node belongs to ring 
is_member(Node, #ring{shards=Shards}) ->
   [X || {_, X} <- Shards, X =:= Node] =/= [].

%%
%% shards(Node, Ring) -> Shards
%%
%% return list of shards owned by ring or node
shards(#ring{shards=Shards}) ->
   Shards.

shards(Node, #ring{shards=Shards}) ->
   [X || {X, N} <- Shards, N =:= Node].

%%
%% whereis(Addr, Ring) -> {Shard, Node}
%%
%% lookup shard/node pair mastering Addr
whereis(Addr, #ring{shards=Shards})
 when is_integer(Addr) ->
   hd(lists:dropwhile(fun({X, _}) -> X < Addr end, Shards)).


% TODO: predecessors with filter fun

%%
%% predecessors(Addr, Ring) -> Nodes
%%
%% return unique list of predecessors 
predecessors(Addr, #ring{n=N, shards=Shards})
 when is_integer(Addr) ->
   {Head, Tail} = lists:partition(
       fun({X, _}) -> X >= Addr end,
       Shards
   ),
   lists:sublist(
      unique(lists:reverse(Tail) ++ lists:reverse(Head)),
      N
   );

predecessors(Key, Ring) ->
   predecessors(address(Key, Ring), Ring).

%%
%% successors(Addr, Ring) -> Nodes
%% 
%% return unique list of successors
successors(Addr, #ring{n=N, shards=Shards})
 when is_integer(Addr) ->
   {Head, Tail} = lists:partition(
       fun({X, _}) -> X >= Addr end,
       Shards
   ),
   lists:sublist(
      unique(Head ++ Tail),
      N
   );

successors(Key, Ring) ->
   successors(address(Key, Ring), Ring).

%%
%% join(Addr, Node, Ring) -> Ring
%%
%% join node to the ring
join(Node, Ring) ->
   join(Node, Node, Ring).

join(Addr, Node, #ring{type=chord}=R)
 when is_integer(Addr) ->
   chord_join(Addr, Node, R);

join(Addr, Node, #ring{type=token}=R)
 when is_integer(Addr) ->
   token_join(Addr, Node, R);

join(Addr, Node, Ring) ->
   join(address(Addr, Ring), Node, Ring).


%%
%% leave(Node, Ring) -> Ring
%%
%% leave node
leave(Node, #ring{type=chord}=R) ->
   chord_leave(Node, R);

leave(Node, #ring{type=token}=R) ->
   token_leave(Node, R).


%%%------------------------------------------------------------------
%%%
%%% chord ring
%%%
%%%------------------------------------------------------------------   

%%
chord_join(Addr, Node, #ring{node=S, shards=Shards}=R) ->
   % new node claim interval from current owner 
   {A, Owner} = whereis(Addr, R),              % start of interval is first shard owner by new node
   {B,     _} = whereis(address(Owner, R), R), % stop of  interval is first shard owner by existed node 
   % check against collisions
   if
      A =:= B -> 
         collision;
      true    ->
         R#ring{
            node   = S + 1,
            shards = chord_claim(A, B, Node, Owner, Shards)
         }
   end.

%%
chord_leave(Node, #ring{type=chord, node=N, shards=Shards}=R) ->
   Owner = hd(predecessors(Node, R)),
   L = lists:map(
      fun
         ({X, Y}) when Y =:= Node -> {X, Owner};
         (X) -> X
      end,
      Shards
   ),
   R#ring{
      node   = N - 1,
      shards = L
   }.

%% claim ring internal
chord_claim(A, B, New, Old, Shards) ->
   lists:map(
      fun
      ({X, N}) when A < B, X >= A, X < B, N =:= Old ->  {X, New};
      ({X, N}) when A > B, X >= A, N =:= Old ->  {X, New};
      ({X, N}) when A > B, X <  B, N =:= Old ->  {X, New};
      ({_, _}=X) -> X
      end,
      Shards 
   ).


%%%------------------------------------------------------------------
%%%
%%% token ring
%%%
%%%------------------------------------------------------------------   

%%
token_join(_, Node, #ring{node=S, shard=Q, shards=Shards}=R) ->
   %T = tokens([Node | members(R)], R), %% give priority of shard to new node
   %% keep priority of shard to old node
   T = tokens(Q, members(R) ++ [Node], R), 
   L = lists:map(
      fun({X, N}) ->
         case lists:keyfind(X, 1, T) of
            false   -> {X, N};
            {_, NN} -> {X, NN}
         end
      end,
      Shards
   ),
   R#ring{
      node   = S + 1,
      shards = L
   }.

 
token_leave(Node, #ring{node=S, shard=Q, shards=Shards}=R) ->
   NShards = lists:filter(fun({_, N}) -> N =/= Node end, Shards),
   T = tokens(2 * Q, lists:usort([X || {_, X} <- NShards]), R),
   L = lists:map(
      fun
      ({X, N}) when N =:= Node ->
         case lists:keyfind(X, 1, T) of
            false   -> throw(unknown_node);
            {_, NN} -> {X, NN}
         end;
      ({_, _}=X) -> X
      end,
      Shards
   ),
   R#ring{
      node   = S - 1,
      shards = L
   }.
 

%%
%% return list of N tokens for each node (ordered by token weight)
tokens(N, Nodes, Ring) ->
   lists:flatten(
      lists:map(
         fun(X) ->
            lists:map(
               fun(Node) ->
                  {Shard,    _} = whereis(address(hash(X, Node), Ring), Ring),
                  {Shard, Node}
               end,
               Nodes
            )
         end,
         lists:seq(1, N)
      )
   ).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   


%%
%%
hash(0, X) ->
   {hash, X};

hash(N, X)
 when is_binary(X) ->
   hash(N - 1, erlang:md5(X));

hash(N, X) ->
   hash(N, term_to_binary(X)).

%%
%% take unique list elements, preserving the order
unique(List) ->
   lists:reverse(
      lists:foldl(
         fun({_, X}, Acc) ->
            case lists:member(X, Acc) of
               true  -> Acc;
               false -> [X | Acc]
            end
         end,
         [],
         List
      )
   ).

% %%
% %% map arc to node map
% arc_to_node(Arc) ->
%    lists:usort(arc_to_node(Arc, [])).

% arc_to_node([{Shard, Node}|T], Acc0) ->
%    case lists:keytake(Node, 1, Acc0) of
%       false -> 
%          arc_to_node(T, [{Node, [Shard]}|Acc0]);
%       {value, {_, List}, Acc} -> 
%          arc_to_node(T, [{Node, [Shard|List]}|Acc])
%    end;

% arc_to_node([], Acc) ->
%    Acc.

% %%
% %% map nodes to shards
% node_to_arc(Nodes) ->
%    lists:usort(
%       lists:flatten(node_to_arc(Nodes, []))
%    ).

% node_to_arc([{Node, []}|_T], _Acc) ->
%    throw({collision, Node});

% node_to_arc([{Node, Shards}|T], Acc) ->
%    node_to_arc(T, [[{X, Node} || X <- Shards] | Acc]);

% node_to_arc([], Acc) ->
%    Acc.


% %%
% %% add new node
% %% partitions are optimally balanced when each node has only one partition on arc.
% add_node(Node, Nodes) ->
%    lists:foldl(
%       fun
%       ({N, Shards}, [{NNode, []} | Acc]) when length(Shards) > 1 ->
%          {Head, Tail} = lists:split(length(Shards) - 1, Shards),
%          [{NNode, Tail}, {N, Head} | Acc];
%       (N, [NewNode|Acc]) ->
%          [NewNode, N | Acc]
%       end,
%       [{Node, []}],
%       Nodes
%    ).

% %%
% %%
% sub_node(Node, Nodes) ->
%    case lists:keytake(Node, 1, Nodes) of
%       false ->
%          Nodes;
%       {value, _, []} ->
%          [];
%       {value, {_, [Shard]}, List} ->
%          [{ONode, Shards} | T] = List,
%          [{ONode, [Shard|Shards]} | T]
%    end.   

% %%
% %% split ring into arcs, 
% arc(N, List) ->
%    arc(N, List, [], []).

% arc(N, List, Arc, Acc) when length(Arc) =:= N ->
%    arc(N, List, [], [Arc|Acc]);

% arc(N, [H|T], Arc, Acc) ->
%    arc(N, T, [H|Arc], Acc);

% arc(_, [], [], Acc) ->
%    Acc;

% arc(_, [], Arc, Acc) ->
%    [Arc | Acc].

