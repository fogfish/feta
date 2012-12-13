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
%% moving average counter
-record(mavg, {
   n   = 0,    % number of samples
   val = 0     % average value
}).

%%
%% elapsed time counter
-record(time, {
	n   = 0,  % number of time intervals
   at  = nil,% time interval start time
   val = 0   % average length of time interval
}).

%%
%% rate per second
-record(rps, {
	n   = 0,    % total number of samples
	at  = nil,  % time interval
   cnt = 0,    % samples count on time interval
	val = 0.0   % average rate (per second)
}).

%%
%% value rate per second
-record(rate, {
   updated = nil, % last update time stamp
   raw     = 0,   % last raw value
   val     = 0.0  % average change rate (per second)
}).

%%
%% new(Type) -> Counter
%%   mavg - moving average counter
%%   time - elapsed time counter
%%   rate - rate counter
%%
%% create a new counter
new(mavg) -> #mavg{};
new(time) -> #time{};
new(rps)  -> #rps{};
new(rate) -> #rate{}.

%%
%% add(Raw, Counter) -> Counter
%%
%% add raw value to counter
add(Raw, #mavg{n=N, val=Val}) ->
   #mavg{n=N + 1, val=(N * Val + Raw) / (N + 1)};

%%
add(now, #time{at=nil}=C) ->
   C#time{at=erlang:now()};

add(now, #time{n=N, at=T0, val=Val}) ->
   Now = erlang:now(),
   Raw = timer:now_diff(Now, T0),
   #time{n=N + 1, at=Now, val=(N * Val + Raw) div (N + 1)}; 

add(idle,#time{n=N, at=T0, val=Val}) ->
   Raw = timer:now_diff(erlang:now(), T0),
   #time{n=N + 1, at=nil, val=(N * Val + Raw) div (N + 1)};

%%
add(Raw, #rps{n=N0, cnt=N1, at=nil}=C) ->
   C#rps{n=N0 + Raw, cnt=N1 + Raw, at=erlang:now()};

add(Raw, #rps{n=N0, cnt=N1, at=T0}=C) ->
   case timer:now_diff(erlang:now(), T0) of
   	T when T > 1000000 ->
   	   Sec = T / 1000000,
   	   C#rps{n=N0 + Raw, cnt=0, at=erlang:now(), val=(N1 + Raw) / Sec};
   	_ ->
			C#rps{n=N0 + Raw, cnt=N1 + Raw}
	end;

%%
add(Raw, #rate{updated=nil}=C) ->
   C#rate{updated=erlang:now(), raw=Raw};

add(Raw, #rate{updated=T0, raw=N}=C)
 when Raw >= N ->
   Sec = timer:now_diff(erlang:now(), T0) / 1000000,
   C#rate{updated=erlang:now(), raw=Raw, val=(Raw - N) / Sec};

add(Raw, #rate{updated=T0, raw=N}=C)
 when Raw < N ->
   C#rate{updated=erlang:now(), raw=Raw}. % reset counter

%%
%% val(Counter) -> Value
%%
%% return counter value
val(#mavg{val=Val}) -> erlang:round(Val);
val(#time{val=Val}) -> Val;
val(#rps{val=Val})  -> Val;
val(#rate{val=Val}) -> Val.

%%
%% return number of observations
len(#mavg{n=N}) -> N;
len(#time{n=N}) -> N;
len(#rps{n=N})  -> N.




