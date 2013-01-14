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
%%
-module(hash).

-export([fnv32/1]).
-export([fnv32a/1]).
-export([fnv32m/1]).
-export([seq31/1, seq32/1]).
-export([fold32/1]).

%%
%%  FNV32 initial state
%%
-define(FNV32_PRIME, 16777619).
-define(FNV32_INIT,  2166136261).
-define(FNV32_MASK,  16#FFFFFFFF).
-define(MASK32,  16#FFFFFFFF).

%%
%% fnv32(Data) -> Hash
%%
fnv32(Data)
 when is_list(Data) ->
	fnv32(list_to_binary(Data), ?FNV32_INIT);

fnv32(Data)
 when is_binary(Data) ->
   fnv32(Data, ?FNV32_INIT).

fnv32(<<H:8, T/bytes>>, State) -> 
	Hash  = ( ( State * ?FNV32_PRIME ) band ?FNV32_MASK ) bxor H,
	fnv32(T, Hash);

fnv32(<<>>, State) -> 
	State.

%%
%%  fnv32a(Data) -> Hash
%%
fnv32a(Data)
 when is_list(Data) ->
	fnv32a(list_to_binary(Data), ?FNV32_INIT);

fnv32a(Data)
 when is_binary(Data) ->
   fnv32a(Data, ?FNV32_INIT).

fnv32a(<<H:8, T/bytes>>, State) -> 
	Hash  =  ( ( State bxor H ) * ?FNV32_PRIME ) band ?FNV32_MASK,
	fnv32a(T, Hash);

fnv32a(<<>>, State) -> 
	State.


%%
%% fnv32m(Data) -> Hash
%% 
%% @see http://home.comcast.net/~bretm/hash/6.html
%%
fnv32m(Data)
 when is_list(Data) ->
	fnv32m(list_to_binary(Data), ?FNV32_INIT);

fnv32m(Data)
 when is_binary(Data) ->
   fnv32m(Data, ?FNV32_INIT).

fnv32m(<<H:8, T/bytes>>, State) -> 
	Hash  =  ( ( State bxor H ) * ?FNV32_PRIME ) band ?FNV32_MASK,
	fnv32m(T, Hash);

fnv32m(<<>>, State) -> 
	Hash1 = (State + (State bsl 13)) band ?FNV32_MASK,
	Hash2 = (Hash1 bxor (Hash1 bsr 7)) band ?FNV32_MASK,
	Hash3 = (Hash2 + (Hash2 bsl 3)) band ?FNV32_MASK,
	Hash4 = (Hash3 bxor (Hash3 bsr 17)) band ?FNV32_MASK,
	Hash5 = (Hash4 + (Hash4 bsl 5)) band ?FNV32_MASK,
	Hash5.

%% Additive congruential method of generating values in 
%% pseudo-random order bt Roy Hann.
%% Initially the shift register contains the value 0001.
%% The two rightmost bits are XOR-ed, the result is fed into
%% the leftmost bit position and previous regsiter contents shift
%% one bit right. Choosing correct bits tap position is important.
%% see E.J. Watson "Primitive Polynomials", Math of Computation
%%  8bit {0, 2, 3, 4}
%% 16bit {0, 2, 3, 5}
%% 31bit {0, 3}
%% 32bit {0, 1, 2, 3, 5, 7}
%% 64bit {0, 1, 3, 4}
seq31(N) ->
   (N bsr 1) bor ((((N bsr 3) bxor N) band 1) bsl 30).

seq32(N) ->
   (N bsr 1) bor ((((N bsr 7) bxor (N bsr 5) bxor (N bsr 3) bxor (N bsr 2) bxor (N bsr 1) bxor N) band 1) bsl 30).


%%
%% fold32
fold32(Data) when is_binary(Data) ->
   fold32(Data, 0).

fold32(<<H:32, T/binary>>, Hash) ->
   fold32(T, Hash bxor H);
fold32(<<H:24>>, Hash) ->
   Hash bxor H;
fold32(<<H:16>>, Hash) ->
   Hash bxor H;
fold32(<<H:8>>, Hash) ->
   Hash bxor H;
fold32(<<>>, Hash) ->
   Hash.




