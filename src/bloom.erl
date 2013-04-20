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
   n    :: integer(),  %% capacity
   p    :: float(),    %% positive false probability 
   m    :: integer(),  %% number of bits
   k    :: integer(),  %% number of hashes
   size :: integer(),  %% number of elements
   bits :: any()       %% bit set 
}).

-define(HASH1(X), erlang:phash2([X], 1 bsl 32)).
-define(HASH2(X), erlang:phash2({X}, 1 bsl 32)).
-define(W,  1024).    %% word size

%%
%% create new bloom filter
-spec(new/2 :: (integer(), float()) -> #bloom{}).

new(N, P) ->
   M = bits(N, P),
   K = hash(N, M),
   #bloom{
      n    = N,
      p    = P,
      m    = M,
      k    = K,
      size = 0,
      bits = array:new(M div ?W , [{default, 0}])
   }.

%%
%%
-spec(insert/2 :: (any(), #bloom{}) -> #bloom{}).

insert(Element, B) ->
   Bits = set_bits(
      [ X rem B#bloom.m || X <- hashes(Element, B#bloom.k) ],
      B#bloom.m div ?W,
      B#bloom.bits
   ),
   B#bloom{
      size = B#bloom.size + 1,
      bits = Bits
   }.

%%
%%
-spec(lookup/2 :: (any(), #bloom{}) -> true | false).

lookup(Element, B) ->
   has_bits(
      [ X rem B#bloom.m || X <- hashes(Element, B#bloom.k) ],
      B#bloom.m div ?W,
      B#bloom.bits
   ).

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%% calculates number of bits for optimal number of hash functions
%% the bits value is normalized to closets word.
bits(N, P) ->
   Bits = - erlang:round(N * math:log(P) / math:pow(math:log(2), 2)),
   ((Bits div ?W) + 1) * ?W.  

%%
%% calculates optimal number of hash functions
hash(N, M) ->
   erlang:round((M / N) * math:log(2)).

%%
%% calculates K hashes
hashes(E, K) ->
   hashes(?HASH1(E), ?HASH2(E), K).

hashes(_, _, 0) ->
   [];
hashes(A, B, K) ->
   [A band (K * B) | hashes(A, B, K - 1)].

%%
%%
set_bits([H | T], Base, Bits) ->
   I = H div Base,
   V0 = array:get(I, Bits),
   V1 = V0 bor (1 bsl (H rem Base)),
   set_bits(T, Base, array:set(I, V1, Bits));

set_bits([], _, Bits) ->
   Bits.

%%
%%
has_bits([H | T], Base, Bits) ->
   I = H div Base,
   V = array:get(I, Bits),
   case V band (1 bsl (H rem Base)) of
      0 -> false;
      _ -> has_bits(T, Base, Bits)
   end;

has_bits([], _, _) ->
   true.


