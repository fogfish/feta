%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%%   standard bloom filter
%%
%%   stable bloom filter
%%      http://webdocs.cs.ualberta.ca/~drafiei/papers/DupDet06Sigmod.pdf
%%
%%   Bloom filters  are build on double-hashing technique
%%      see http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
-module(bloom).
-define(TESTS,  tests).

-export([
   new/2, 
   new/3,
   start_link/1,
   start_link/2,
   insert/2, 
   lookup/2,
   % gen_server
   init/1, 
   terminate/2,
   handle_call/3, 
   handle_cast/2, 
   handle_info/2,  
   code_change/3
]).

%%
-record(bloom, {
   type :: atom(),     %% filter type
   e    :: float(),    %% positive false probability 
   n    :: integer(),  %% filter capacity
   k    :: integer(),  %% number of hashes
   m    :: integer(),  %% number of bits
   size :: integer(),  %% number of elements
   bits :: any()       %% bit set 
}).

-define(HASH1(X), erlang:phash2([X], 1 bsl 32)).
-define(HASH2(X), erlang:phash2({X}, 1 bsl 32)).
-define(BITSET,   hipe).

%%
%% create new bloom filter
%%   N - estimated size of the set
%%   E - desired false positive probability
-spec(new/2 :: (integer(), float()) -> #bloom{}).
-spec(new/3 :: (atom(), integer(), float()) -> #bloom{}).

new(N, E) ->
   new(std, N, E).

new(std, N, E) ->
   %% estimate optimal number of hash functions (fill rate 50%)
   K = 1 + erlang:trunc(log2(1 / E)),
   %% estimate false probability
   P = math:pow(E, 1 / K),
   %% estimate filter size in bits
   M = 1 - erlang:round(N * math:log(P) / math:pow(math:log(2), 2)),
   #bloom{
      type = std,
      e    = E,
      n    = N,
      k    = K,
      m    = M,
      size = 0,
      bits = bitset:new(?BITSET, M)
   };

new(stable, N, E) ->
   %% estimate optimal number of hash functions (fill rate 50%)
   K = 1 + erlang:trunc(log2(1 / E)),
   %% estimate false probability
   P = math:pow(E, 1 / K),
   %% estimate filter size in bits
   M = 1 - erlang:round(N * math:log(P) / math:pow(math:log(2), 2)),
   %% estimate P number of cells we pick to decrement by 1 in each iteration (Max = 1 bit) ~ K
   %% C = 1 + erlang:trunc( 1 / (( 1 / (1 - P) - 1 ) * (1 / K)) ).
   #bloom{
      type = stable,
      e    = E,
      n    = N,
      k    = K,
      m    = M,
      size = 0,
      bits = bitset:new(?BITSET, M)
   }.

%%
%% create new bloom filter (wrapped into process)
%%   Options
%%     {type,    atom()} - bloom filter type
%%     {n,    integer()} - estimated size of the set
%%     {e,      float()} - desired false positive probability   
-spec(start_link/1 :: (list()) -> {ok, pid()} | {error, any()}).
-spec(start_link/2 :: (atom(), list()) -> {ok, pid()} | {error, any()}).

start_link(Opts) ->
   gen_server:start_link(?MODULE, [Opts], []).

start_link(Name, Opts) ->
   gen_server:start_link({local, Name}, ?MODULE, [Opts], []).


%%
%%
-spec(insert/2 :: (any(), #bloom{}) -> #bloom{}).

insert(Element, #bloom{}=B) ->
   B#bloom{
      size = B#bloom.size + 1,
      bits = bits_set(hashes(Element, B#bloom.k, B#bloom.m), B#bloom.bits)
   };
insert(Element, Pid) ->
   ok = gen_server:call(Pid, {insert, Element}),
   Pid.

%%
%%
-spec(lookup/2 :: (any(), #bloom{}) -> {true | false, #bloom{}}).

lookup(Element, #bloom{type=std}=B) ->
   {
      bits_has(hashes(Element, B#bloom.k, B#bloom.m), B#bloom.bits),
      B
   };
lookup(Element, #bloom{type=stable}=B) ->
   Random = random:uniform(16#ffffffff),
   {
      bits_has(hashes(Element, B#bloom.k, B#bloom.m), B#bloom.bits),
      B#bloom{
         size = B#bloom.size - 1,
         bits = bits_rst(hashes(Random, B#bloom.k, B#bloom.m), B#bloom.bits)
      }
   };
lookup(Element, Pid) ->
   {ok, Result} = gen_server:call(Pid, {lookup, Element}),
   {Result, Pid}.

%%%------------------------------------------------------------------
%%%
%%% gen_server
%%%
%%%------------------------------------------------------------------

%%
%%
init([Opts]) ->
   {ok, bloom:new(opts:val(type, std, Opts), opts:val(n, Opts), opts:val(e, Opts))}.

%%
%%
terminate(_Reason, _) ->
   ok.

%%
%%
handle_call({insert, Element}, _, S) ->
   {reply, ok, bloom:insert(Element, S)};

handle_call({lookup, Element}, _, S) ->
   {Result, Bloom} = bloom:lookup(Element, S),
   {reply, {ok, Result}, Bloom};

handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _) ->
   {ok, S}.


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
log2(X) -> math:log(X) / math:log(2). 


%%
%% calculates K hashes 
%% double hashing technique is defined at
%% http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
hashes(X, K, M) ->
   hashes(?HASH1(X), ?HASH2(X), K, M).
hashes(_, _, 0, _) ->
   [];
hashes(A, B, K, M) ->
   %% @todo: optimize multiplication
   [ (A band (K * B)) rem M | hashes(A, B, K - 1, M)].

%%
%%
bits_set([H | T], Bits) ->
   bits_set(T, bitset:set(H, Bits));
bits_set([], Bits) ->
   Bits.

%%
%%
bits_rst([H | T], Bits) ->
   bits_rst(T, bitset:rst(H, Bits));
bits_rst([], Bits) ->
   Bits.


%%
%%
bits_has([H | T], Bits) ->
   case bitset:get(H, Bits) of
      true  -> bits_has(T, Bits);
      Value -> Value
   end;
bits_has([], _) ->
   true.

%%%------------------------------------------------------------------
%%%
%%% unit test
%%%
%%%------------------------------------------------------------------
-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").

insert_set(List, Bloom) ->
   lists:foldl(fun(X, Acc) -> bloom:insert(X, Acc) end, Bloom, List).

lookup_positive(List, Bloom) ->
   lists:foldl(fun(X, Acc0) -> {true,  Acc} = bloom:lookup(X, Acc0), Acc end, Bloom, List).

lookup_negative(List, Bloom) ->
   lists:foldl(fun(X, Acc0) -> {false, Acc} = bloom:lookup(X, Acc0), Acc end, Bloom, List).


std_bloom_test() ->
   Set   = lists:seq(0, 100),
   Bloom = insert_set(Set, bloom:new(std, 1000, 0.001)),

   lookup_positive(Set, Bloom),
   lookup_negative(lists:seq(200, 300), Bloom).

-endif.

