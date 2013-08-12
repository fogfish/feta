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
   % events
   event/2,
   reset/2,
   cancel/1,

   % time convert utility
   now/0, 
   sec/0, 
   sec/1, 
   milli/0, 
   milli/1, 
   micro/0, 
   micro/1,

   % time transform utility
   inc/1, 
   inc/2, 
   dec/1, 
   dec/2 
]).

%% number of sec to Unix Epoch
-define(UNX_EPOCH, 62167219200).


%%%------------------------------------------------------------------
%%%
%%% events
%%%
%%%------------------------------------------------------------------

%%
%% raise event after timeout
-spec(event/2 :: (integer() | any(), any()) -> any()).

event(T, Evt)
 when is_integer(T) ->
   {evt, T, erlang:send_after(T, self(), Evt)};

event({evt, _, _}=T, _Evt) ->
   T;

event(T, _) ->
   T.

%%
%% reset event's timeout 
-spec(reset/2 :: (integer() | any(), any()) -> any()).

reset(T, Msg)
 when is_integer(T) ->
   tempus:event(T, Msg);

reset(T, Evt) ->
   tempus:event(tempus:cancel(T), Evt).

%%
%% cancel event
-spec(cancel/1 :: (any()) -> integer()).

cancel(T)
 when is_integer(T) ->
   T;

cancel({evt, T, Timer}) ->
   erlang:cancel_timer(Timer),
   T;

cancel(T) ->
   T.




%%%------------------------------------------------------------------
%%%
%%% convert
%%%
%%%------------------------------------------------------------------

%%
%% current time in seconds
now() -> 
   sec().

%%
%% current time in seconds
sec() -> 
   sec(os:timestamp()).

%%
%% convert time to second
sec({Msec, Sec, _Usec}) ->
   % erlang:now
   Msec * 1000000 + Sec;
sec({{_,_,_},{_,_,_}}=Date) ->
   % erlang date/time
   calendar:datetime_to_gregorian_seconds(Date) - ?UNX_EPOCH;
sec(Sec)
 when is_list(Sec), length(Sec) =:= 8 ->
   % any 8-length time is considered to be YYYYMMDD format
   % 1970 - 1974 dates has to be requested as iso8601 or use scalar utility
   sec(parser:iso8601(Sec));
sec(T)
 when is_list(T) ->
   case string:chr(T, $T) of
      0 -> 
         case scalar:decode(T) of
            X when is_list(X) -> sec(list_to_existing_atom(X));
            X -> sec(X)
         end;
      _ -> sec(parser:iso8601(T))
   end;
sec(T)
 when is_binary(T) ->
   sec(binary_to_list(T));
sec(today) ->
   86400;
sec(day)   ->
   86400;
sec(week)  -> 
   7  * 86400;
sec(month) -> 
   30 * 86400;
sec(year)  -> 
   365 * 86400;
sec(hour)  ->
    3600;
sec(Sec)
 when is_float(Sec) ->
   sec(erlang:round(Sec));
sec(Sec)
 when is_integer(Sec), Sec < 9999999999 ->
   Sec; 
sec(Milli)
 when is_integer(Milli) ->
   % given time value exceeds max allowed Sec value
   Milli div 1000.

%%
%% current time in milliseconds   
milli() ->
   milli(os:timestamp()).

milli({Msec, Sec, Usec}) ->
   (Msec * 1000000 + Sec) * 1000 + Usec div 1000.

%%
%% current time in microseconds
micro() ->
   micro(os:timestamp()).

micro({Msec, Sec, Usec}) ->
   (Msec * 1000000 + Sec) * 1000000 + Usec.

%%%------------------------------------------------------------------
%%%
%%% transform
%%%
%%%------------------------------------------------------------------

%%
%% increase time by T seconds
inc(T) ->
   inc(os:timestamp(), sec(T)).

inc({Msec, Sec, Usec}, T)
 when is_integer(T) ->
   case Sec + T of
      X when X =< 1000000 ->
         {Msec, X, Usec};
      X ->
         {Msec + (X div 1000000), X rem 1000000, Usec}
   end.

%%
%% increase time by T seconds
dec(T) ->
   dec(os:timestamp(), sec(T)).

dec({Msec, Sec, Usec}, T)
 when is_integer(T) ->
   case Sec - T of
      X when X >= 0 ->
         {Msec, X, Usec};
      X ->
         {Msec - 1, 1000000 + X, Usec}
   end.
