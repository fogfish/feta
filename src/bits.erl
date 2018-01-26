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
-export([
   cnt32/1
  ,lzc32/1
]).

%% bitstring 
-export([
   cnt/1
  ,lzc/1
  ,lcc/2
  ,prefix/2
  ,is_prefix/2
]).

-export([pxor/2, pand/2]).
-export([cast/1, btoi/1, btol/1, btoh/1, htob/1]).
-export([zip32/2, zip64/2, zip64/3]).

%%%----------------------------------------------------------------------------   
%%%
%%% 32 bit integer
%%%
%%%----------------------------------------------------------------------------   

%%
%% count number of set bit at 32 bit integer (ones, count)
-spec cnt32(integer()) -> integer().

cnt32(X0) ->
   X1 = X0 - ((X0 bsr 1) band 16#55555555),
   X2 = (((X1 bsr 2) band 16#33333333) + (X1 band 16#33333333)),
   X3 = (((X2 bsr 4) + X2) band 16#0f0f0f0f),
   X4 = X3 + (X3 bsr  8),
   X5 = X4 + (X4 bsr 16),
   X5 band 16#0000003f.

%%
%% leading zero count at 32 bit integer
-spec lzc32(integer()) -> integer().

lzc32(X0) ->
   X1 = X0 bor (X0 bsr  1),
   X2 = X1 bor (X1 bsr  2),
   X3 = X2 bor (X2 bsr  4),
   X4 = X3 bor (X3 bsr  8),
   X5 = X4 bor (X4 bsr 16),
   32 - cnt32(X5).

%%%----------------------------------------------------------------------------   
%%%
%%% binary
%%%
%%%----------------------------------------------------------------------------   

%%
%% count number of set bit
-spec cnt(bitstring()) -> integer().

cnt(X)
 when is_bitstring(X) ->
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
-spec lzc(bitstring()) -> integer().

lzc(X)
 when is_bitstring(X) ->
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
%% leading common bit count
-spec lcc(bitstring(), bitstring()) -> integer().

lcc(X, Y)
 when is_bitstring(X), is_bitstring(Y) ->
   Xlen = bit_size(X),
   Ylen = bit_size(Y),
   Size = if Xlen > Ylen -> Ylen ; true -> Xlen end,
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   lzc(<<(A bxor B):Size>>).


%% 
%% common bits prefix
-spec prefix(bitstring(), bitstring()) -> bitstring().

prefix(X, Y)
 when is_bitstring(X), is_bitstring(Y) ->
   Plen = lcc(X,Y),
   <<Prefix:Plen/bits, _/bits>> = X,
   Prefix. 

%%
%% return true if Y is prefix of X
-spec is_prefix(bitstring(), bitstring()) -> true | false.

is_prefix(X, Y)
 when is_bitstring(X), is_bitstring(Y) ->
   Len = bit_size(Y),
   case X of
      <<Y:Len/bits, _/bits>> ->
         true;
      _ ->
         false
   end. 


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
%% cast erlang scalar type to binary
cast(X)
 when is_binary(X) ->
   X;
cast(X)
 when is_atom(X) ->
   atom_to_binary(X, utf8);
cast(X)
 when is_integer(X) ->
   list_to_binary(integer_to_list(X));
cast(X)
 when is_float(X) ->
   list_to_binary(float_to_list(X));
cast(X)
 when is_list(X) ->
   list_to_binary(X).

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
   << <<(if A >= $a, A =< $f -> 10 + (A - $a); A >= $A, A =< $F -> 10 + (A - $A); A >=$0, A =< $9 -> A - $0 end):4>> || <<A:8>> <=X >>. 


%%
%%
zip32(X, Y) ->
   zip32i(X) bor (zip32i(Y) bsl 1).

zip32i(X0) ->
   X1 = (X0 bxor (X0 bsl 16)) band 16#0000ffff0000ffff,
   X2 = (X1 bxor (X1 bsl 8 )) band 16#00ff00ff00ff00ff,
   X3 = (X2 bxor (X2 bsl 4 )) band 16#0f0f0f0f0f0f0f0f,
   X4 = (X3 bxor (X3 bsl 2 )) band 16#3333333333333333,
   X5 = (X4 bxor (X4 bsl 1 )) band 16#5555555555555555,
   X5.   

zip64(X, Y) ->
   zip64i(X) bor (zip64i(Y) bsl 1).

zip64i(X0) ->
   X1 = (X0 bxor (X0 bsl 32)) band 16#00000000ffffffff00000000ffffffff,
   X2 = (X1 bxor (X1 bsl 16)) band 16#0000ffff0000ffff0000ffff0000ffff,
   X3 = (X2 bxor (X2 bsl 8 )) band 16#00ff00ff00ff00ff00ff00ff00ff00ff,
   X4 = (X3 bxor (X3 bsl 4 )) band 16#0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f,
   X5 = (X4 bxor (X4 bsl 2 )) band 16#33333333333333333333333333333333,
   X6 = (X5 bxor (X5 bsl 1 )) band 16#55555555555555555555555555555555,
   X6.

zip64(X, Y, Z) ->
   zip64i3(X) bor (zip64i3(Y) bsl 1) bor (zip64i3(Z) bsl 2).


zip64i3(X0) ->
   X1 = X0 band 16#3ffffffffff,
   X2 = (X1 bor (X1 bsl 64 )) band 16#00003ff0000000000000000ffffffff,
   X3 = (X2 bor (X2 bsl 32 )) band 16#00003ff00000000ffff00000000ffff,
   X4 = (X3 bor (X3 bsl 16 )) band 16#30000ff0000ff0000ff0000ff0000ff,
   X5 = (X4 bor (X4 bsl  8 )) band 16#300f00f00f00f00f00f00f00f00f00f,
   X6 = (X5 bor (X5 bsl  4 )) band 16#30c30c30c30c30c30c30c30c30c30c3,
   X7 = (X6 bor (X6 bsl  2 )) band 16#9249249249249249249249249249249,
   X7.   
