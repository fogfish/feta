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
%%    (see http://aggregate.org/MAGIC)
-module(bits).

%% 32bit primitives
-export([cnt32/1, lzc32/1]).
%% bitstring 
-export([cnt/1, lzc/1, lcc/2, prefix/2]).

-export([pxor/2, pand/2]).
-export([btoi/1, btol/1, btoh/1, htob/1]).
-export([interleave/2]).


%%
%% set bit count (ones, count)
cnt32(X0) ->
   X1 = X0 - ((X0 bsr 1) band 16#55555555),
   X2 = (((X1 bsr 2) band 16#33333333) + (X1 band 16#33333333)),
   X3 = (((X2 bsr 4) + X2) band 16#0f0f0f0f),
   X4 = X3 + (X3 bsr  8),
   X5 = X4 + (X4 bsr 16),
   X5 band 16#0000003f.

%%
%% leading zero count
lzc32(X0) ->
   X1 = X0 bor (X0 bsr  1),
   X2 = X1 bor (X1 bsr  2),
   X3 = X2 bor (X2 bsr  4),
   X4 = X3 bor (X3 bsr  8),
   X5 = X4 bor (X4 bsr 16),
   32 - cnt32(X5).


%%
%% set bit count
cnt(X) when is_bitstring(X) ->
   cnt(X, 0).

cnt(<<X:32, Y/bits>>, Acc) ->
   cnt(Y, Acc + cnt32(X));
cnt(<<X/bits>>, Acc) ->
   Size = bit_size(X),
   <<A:Size>> = X,
   Acc + cnt32(A) - (32 - Size);
cnt(<<>>, Acc) ->
   Acc.

%%
%% leading zero count
lzc(X) when is_bitstring(X) ->
   lzc(X, 0).

lzc(<<0:32, Y/bits>>, Acc) ->
   lzc(Y, Acc + 32);
lzc(<<X:32, _/bits>>, Acc) ->
   Acc + lzc32(X);
lzc(<<X/bits>>, Acc) ->
   Size = bit_size(X),
   <<A:Size>> = X,
   Acc + lzc32(A) - (32 - Size);
lzc(<<>>, Acc) ->
   Acc.

%%
%% lcc(X, Y) -> Count
%%
%% leading common bit count
lcc(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Xlen = bit_size(X),
   Ylen = bit_size(Y),
   Size = if Xlen > Ylen -> Ylen ; true -> Xlen end,
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   lzc(<<(A bxor B):Size>>).


%%
%% prefix(X, Y) -> Bits
%% 
%% common bits prefix
prefix(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Plen = lcc(X,Y),
   <<Prefix:Plen/bits, _/bits>> = X,
   Prefix. 

%%
%%
pxor(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Xlen = bit_size(X),
   Ylen = bit_size(Y),
   Size = if Xlen > Ylen -> Ylen ; true -> Xlen end,
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   <<(A bxor B):Size>>.

%%
%%
pand(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Xlen = bit_size(X),
   Ylen = bit_size(Y),
   Size = if Xlen > Ylen -> Ylen ; true -> Xlen end,
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   <<(A band B):Size>>.

%%
%% btoi(X) -> integer()
%%
%% binary to integer
btoi(X) ->
   Size = bit_size(X),
   <<A:Size>> = X,
   A. 

%%
%% btol(X) -> list()
%%
%% binary to list
btol(X) ->
   binary_to_list(<< <<($0 + A):8>> || <<A:1>> <= X >>).

%%
%% btoh(X) -> binary()
%%   
%% binary to hexadecimal
btoh(X) ->
   << <<(if A < 10 -> $0 + A; A >= 10 -> $a + (A - 10) end):8>> || <<A:4>> <=X >>.

%%
%% htob(X) -> binary()
%%
%% hexadecimal to binary
htob(X) ->
   << <<(if A >= $a, A =< $f -> 10 + (A - $a); A >=$0, A =< $9 -> A - $0 end):4>> || <<A:8>> <=X >>. 

%%
%%
interleave(X, Y) 
 when is_bitstring(X), is_bitstring(Y), 
      bit_size(X) =:= bit_size(Y) ->
   interleave(X, Y, bit_size(X) - 1, <<>>).

interleave(_, _, -1, Acc) ->
   Acc;
interleave(X, Y, N, Acc) ->
   <<_:N/bits, Xbit:1, _/bits>> = X,
   <<_:N/bits, Ybit:1, _/bits>> = Y,
   interleave(X, Y, N - 1, <<Xbit:1, Ybit:1, Acc/bits>>).