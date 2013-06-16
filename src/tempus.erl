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
   sec/0, milli/0, micro/0
]).

%%
%% current time in seconds
sec() ->
   {Msec, Sec, _Usec} = erlang:now(),
   Msec * 1000000 + Sec.

%%
%% current time in milliseconds   
milli() ->
   {Msec, Sec, Usec} = erlang:now(),
   (Msec * 1000000 + Sec) * 1000 + Usec div 1000.

%%
%% current time in microseconds
micro() ->
   {Msec, Sec, Usec} = erlang:now(),
   (Msec * 1000000 + Sec) * 1000000 + Usec.



