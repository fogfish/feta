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
%%   @description
%%      generate artificial time series signal
%%      http://www.it.uu.se/research/group/udbl/Theses/HenrikAndreJonssonPhD.pdf
-module(signal).

-export([new/1, new/2, value/1]).

%%
-record(signal, {
   state = slope,  % signal state
   a,              % signal amplitude
   b,              % 
   c,
   d,
   alpha = 0,
   beta  = 0, % next level to move data
   gamma = 0, % number of steps to reach beta
   delta = 0, % number of steps for stable signal
   cnt   = 0, 
   val   = 0
}).

-define(PERIOD,      30). %% period is 30 points
-define(SLOPE,      0.7). %% 70% signal is slope
-define(STABLE,     0.3). %% 30% signal is stable
-define(VAR,       0.05).

%%
%% new(Val, Cycle) -> Signal
%%    Val   = integer(), mean value
%%    Cycle = integer(), length of cycle
new(Max) ->
   new(Max, []).

new(Max, Opts) ->
   Period = proplists:get_value(period, Opts, ?PERIOD),
   Slope  = proplists:get_value(slope,  Opts, ?SLOPE),
   Stable = proplists:get_value(stable, Opts, ?STABLE),
   Var    = proplists:get_value(var,    Opts, ?VAR),
   B      = erlang:round(Period * Slope),
   #signal{
   	a = Max,
   	b = B,
   	c = erlang:round(Period * Stable),
   	d = erlang:round(Max    * Var),
      beta  = rand:uniform(Max),
      gamma = rand:uniform(B)
   }.

%%
%%
value(#signal{state=slope, alpha=A, beta=B, gamma=G, cnt=Cnt, val=V0}=S) when Cnt < G ->
   % make a slope
   Val = erlang:round(V0 + (B - A) / G),
   {Val, S#signal{val=Val, cnt=Cnt + 1}};

value(#signal{state=slope, c=C}=S) ->
   % slope is over make a stable part
   value(S#signal{
      state = stable,
      delta = rand:uniform(C),
      cnt   = 0
   });

value(#signal{state=stable, d=D0, delta=D, cnt=Cnt, val=V0}=S) when Cnt < D ->
   % make a stable part
   Val = erlang:round(V0 + rand:uniform(D0)),
   {Val, S#signal{val=Val, cnt=Cnt + 1}};

value(#signal{state=stable, a=A0, b=B0, beta=B}=S) ->
   % stable part is over
   value(S#signal{
      state = slope,
      cnt   = 0,
      val   = B,
      alpha = B,
      beta  = rand:uniform(A0),
      gamma = rand:uniform(B0) 
   }).




