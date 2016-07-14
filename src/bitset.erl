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
%%   abstract interface to bitsets
-module(bitset).
-define(TESTS, tests).

-export([
	new/1,
	new/2,
	set/2,
	rst/2,
	get/2
]).
-export_type([bitset/0]).

%%
%%
-define(WORD_ARRAY,  128). % tuned for space / performance
-define(WORD_ETS,     32). % tuned for performance

%%
-record(bitset, {
	type,
	bits
}).

%%
-type(bitset() :: #bitset{}).

%%
%% create new bitset
-spec new(integer()) -> bitset().
-spec new(atom(), integer()) -> bitset().

new(Size) ->
	new(hipe, Size).

new(array, Size) ->
	#bitset{
		type = array,
		bits = array:new(1 + erlang:trunc(Size / ?WORD_ARRAY), [{default, 0}])
	};

new(hipe,  Size) ->
	#bitset{
		type = hipe,
		bits = hipe_bifs:bitarray((1 + erlang:trunc(Size / 8)) * 8, false)
	};

new(ets,   _Size) ->
	_ = ensure_bitset_ets(),
	#bitset{
		type = ets,
		bits = erlang:make_ref()
	}.	

%%
%% set bit
-spec set(integer(), bitset()) -> bitset().

set(Bit, #bitset{type=array, bits=Bits}=S) ->
	Index = Bit div ?WORD_ARRAY,
	Value = array:get(Index, Bits),
  	Result= Value bor (1 bsl (Bit rem ?WORD_ARRAY)),
  	S#bitset{
  		bits = array:set(Index, Result, Bits)
  	};

set(Bit, #bitset{type=hipe, bits=Bits}=S) ->
	S#bitset{
		bits = hipe_bifs:bitarray_update(Bits, Bit, true)
	};

set(Bit, #bitset{type=ets, bits=Bits}=S) ->
	Index = Bit div ?WORD_ETS,
	Value = case ets:lookup(bitsets, {Bits, Index}) of
		[]         -> 0;
		[{_, Val}] -> Val
	end,
	Result= Value bor (1 bsl (Bit rem ?WORD_ETS)),
	_     = ets:insert(bitsets, {{Bits, Index}, Result}),
	S.

%%
%% reset bit
-spec rst(integer(), bitset()) -> bitset().

rst(Bit, #bitset{type=array, bits=Bits}=S) ->
	Index = Bit div ?WORD_ARRAY,
	Value = array:get(Index, Bits),
  	Result= Value band bnot (1 bsl (Bit rem ?WORD_ARRAY)),
  	S#bitset{
  		bits = array:set(Index, Result, Bits)
  	};

rst(Bit, #bitset{type=hipe, bits=Bits}=S) ->
	S#bitset{
		bits = hipe_bifs:bitarray_update(Bits, Bit, false)
	};

rst(Bit, #bitset{type=ets, bits=Bits}=S) ->
	Index = Bit div ?WORD_ETS,
	Value = case ets:lookup(bitsets, {Bits, Index}) of
		[]         -> 0;
		[{_, Val}] -> Val
	end,
	Result= Value band bnot (1 bsl (Bit rem ?WORD_ETS)),
	_     = ets:insert(bitsets, {{Bits, Index}, Result}),
	S.


%%
%%
-spec get(integer(), bitset()) -> true | false.

get(Bit, #bitset{type=array, bits=Bits}) ->
	Index = Bit div ?WORD_ARRAY,
	Value = array:get(Index, Bits),
	Value band (1 bsl (Bit rem ?WORD_ARRAY)) =/= 0;

get(Bit, #bitset{type=hipe, bits=Bits}) ->
	hipe_bifs:bitarray_sub(Bits, Bit);

get(Bit, #bitset{type=ets, bits=Bits}) ->
   Index = Bit div ?WORD_ETS,
	case ets:lookup(bitsets, {Bits, Index}) of
		[] -> 
			false;
		[{_, Value}] ->
		   Value band (1 bsl (Bit rem ?WORD_ETS)) =/= 0
	end.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
ensure_bitset_ets() ->
	case ets:info(bitsets) of
		undefined ->
			ets:new(bitsets, [public, set, named_table]);
		_ ->
			ok
	end.

-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").

-define(N, 10000).

array_test() ->
	{T, _} = timer:tc(fun() -> test(array, ?N) end),
	error_logger:error_report([{bitset_array, T}]).

hipe_test() ->
	{T, _} = timer:tc(fun() -> test(hipe, ?N) end),
	error_logger:error_report([{bitset_hipe, T}]).

ets_test() ->
	{T, _} = timer:tc(fun() -> test(ets, ?N) end),
	error_logger:error_report([{bitset_ets, T}]).

%%
%%
test(Type, N) ->
	lists:foldl(
		fun(X, Acc0) ->
			Acc1 = bitset:set(X, Acc0),
			true = bitset:get(X, Acc1),
			Acc1
		end,
		bitset:new(Type, N),
		lists:seq(1, N)
	).

-endif.

