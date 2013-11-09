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
%%   @description
%%      finite bloom filter build on double-hashing technique
%%      see http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
-module(bloom).

-export([
   new/2, insert/2, lookup/2
]).

%%
-record(bloom, {
   e    :: float(),    %% positive false probability 
   n    :: integer(),  %% capacity
   k    :: integer(),  %% number of hashes
   m    :: integer(),  %% number of bits
   base :: integer(),  %% cell base
   size :: integer(),  %% number of elements
   bits :: any()       %% bit set 
}).

-define(HASH1(X), erlang:phash2([X], 1 bsl 32)).
-define(HASH2(X), erlang:phash2({X}, 1 bsl 32)).
-define(W,  1024).     %% word size

%%
%% create new standard bloom filter
%%   N - estimated size of the set
%%   E - desired false positive probability
-spec(new/2 :: (integer(), float()) -> #bloom{}).

new(N, E) ->
   %% estimate optimal number of hash functions (fill rate 50%)
   K = 1 + erlang:trunc(log2(1 / E)),
   %% estimate false probability
   P = math:pow(E, 1 / K),
   %% estimate filter size in bits
   M = 1 - erlang:round(N * math:log(P) / math:pow(math:log(2), 2)),
   #bloom{
      e    = E,
      n    = N,
      k    = K,
      m    = M,
      base = M div ?W,
      size = 0,
      bits = array:new(1 + erlang:trunc(M / ?W), [{default, 0}])
   }.

%%
%%
-spec(insert/2 :: (any(), #bloom{}) -> #bloom{}).

insert(Element, B) ->
   Bits = set_bits(hashes(Element, B#bloom.k, B#bloom.m), B#bloom.base, B#bloom.bits),
   B#bloom{
      size = B#bloom.size + 1,
      bits = Bits
   }.

%%
%%
-spec(lookup/2 :: (any(), #bloom{}) -> true | false).

lookup(Element, B) ->
   has_bits(hashes(Element, B#bloom.k, B#bloom.m), B#bloom.base, B#bloom.bits).

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
%% @todo: check reference for K hash construction
hashes(E, K, M) ->
   hashes(?HASH1(E), ?HASH2(E), K, M).
hashes(_, _, 0, _) ->
   [];
hashes(A, B, K, M) ->
   [ (A band (K * B)) rem M | hashes(A, B, K - 1, M)].

%%
%%
set_bits([H | T], Base, Bits) ->
   I = H rem array:size(Bits),
   V0 = array:get(I, Bits),
   V1 = V0 bor (1 bsl (H rem Base)),
   set_bits(T, Base, array:set(I, V1, Bits));

set_bits([], _, Bits) ->
   Bits.

%%
%%
has_bits([H | T], Base, Bits) ->
   I = H rem array:size(Bits),
   V = array:get(I, Bits),
   case V band (1 bsl (H rem Base)) of
      0 -> false;
      _ -> has_bits(T, Base, Bits)
   end;

has_bits([], _, _) ->
   true.


