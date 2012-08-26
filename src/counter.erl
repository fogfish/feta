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
%%     generic counter interface
-module(counter).

-export([new/1, add/2, val/1, len/1]).

%%
%% new(Type) -> Counter
%%   mavg - moving average counter
%%   time - elapsed time counter
%%
%% create a new counter
new(mavg) -> {mavg, 0, 0};
new(time) -> {time, 0, 0, nil}.

%%
%% add(Raw, Counter) -> Counter
%%
%% add raw value to counter
add(Raw, {mavg, N, Val}) ->
   {mavg, N + 1, (N * Val + Raw) / (N + 1)};

add(now, {time, N, Val,  nil}) ->
   {time, N, Val, erlang:now()};
add(now, {time, N, Val, T0}) ->
   Now = erlang:now(),
   Raw = timer:now_diff(Now, T0),
   {time, N + 1, (N * Val + Raw) div (N + 1), Now};   
add(idle,{time, N, Val, T0}) ->
   Raw = timer:now_diff(erlang:now(), T0),
   {time, N + 1, (N * Val + Raw) div (N + 1), nil}.

%%
%% val(Counter) -> Value
%%
%% return counter value
val({mavg, _, Val})    -> erlang:round(Val);
val({time, _, Val, _}) -> Val.

%%
%% return number of observations
len({mavg, N, _})    -> N;
len({time, N, _, _}) -> N.




