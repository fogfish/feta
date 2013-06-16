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
%%      time utility
-module(tempus).

-export([
   sec/0,   sec/1, now/0, 
   milli/0, milli/1, 
   micro/0, micro/1,

   shift/1, shift/2 
]).

%%
%% current time in seconds
now() -> 
   sec().

%%
%% current time in seconds
sec() -> 
   sec(erlang:now()).

sec({Msec, Sec, _Usec}) ->
   Msec * 1000000 + Sec.

%%
%% current time in milliseconds   
milli() ->
   milli(erlang:now()).

milli({Msec, Sec, Usec}) ->
   (Msec * 1000000 + Sec) * 1000 + Usec div 1000.

%%
%% current time in microseconds
micro() ->
   micro(erlang:now()).

micro({Msec, Sec, Usec}) ->
   (Msec * 1000000 + Sec) * 1000000 + Usec.

%%
%% add seconds
shift(T) ->
   shift(erlang:now(), T).

shift({Msec, Sec, Usec}, T)
 when is_integer(T) ->
   A = (Sec + T) rem 1000000,
   B = (Sec + T) div 1000000,
   {Msec + B, A, Usec}.

