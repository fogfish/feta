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
-module(bits).

-export([lzc/1, lzc/2, lcc/2, prefix/2]).
-export([pxor/2, pand/2]).
-export([btoi/1, btol/1, btoh/1, htob/1]).

%%
%% lzc(Bits)     -> Count
%% lzc(N, Width) -> Count
%%
%% leading zero count
lzc(X) when is_bitstring(X) ->
   Size = bit_size(X),
   <<A:Size>> = X,
   lzc(A, Size).

lzc(0, Width) ->
   Width;
lzc(X, Width) when is_integer(X) ->
   lzc(X bsr 1, Width - 1).

%%
%% lcc(X, Y) -> Count
%%
%% leading common bit count
lcc(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Size = min(bit_size(X), bit_size(Y)),
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   lzc(A bxor B, Size).


%%
%% prefix(X, Y) -> Bits
%% 
%% common bits prefix
prefix(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   prefix8(X, Y, <<>>).

prefix8(<<A:8, XR/bits>> = X, <<B:8, YR/bits>> = Y, Acc) ->
   case (A bxor B) of
      0 -> prefix8(XR, YR, <<Acc/bits, A:8>>);
      _ -> prefix1(X, Y, Acc)
   end;
prefix8(X, Y, Acc) ->
   prefix1(X, Y, Acc). 

prefix1(<<A:1, XR/bits>>, <<B:1, YR/bits>>, Acc) ->
   case (A bxor B) of
      0 -> prefix1(XR, YR, <<Acc/bits, A:1>>);
      _ -> Acc
   end;
prefix1(_, _, Acc) ->
   Acc. 


%%
%% pxor(X, Y) -> Z
%%
%% perform XOR
pxor(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Size = min(bit_size(X), bit_size(Y)),
   <<A:Size, _/bits>> = X,
   <<B:Size, _/bits>> = Y,
   <<(A bxor B):Size>>.

%%
%% pand(X, Y) -> Z
%%
%% perform AND
pand(X, Y) when is_bitstring(X), is_bitstring(Y) ->
   Size = min(bit_size(X), bit_size(Y)),
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

