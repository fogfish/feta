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
%%   time utility
%%
%% @todo
%%   * optimize arithmetic
%%
-module(tempus).

-export([
   u/1,
   m/1,
   s/1,

   add/2,
   sub/2,
   seq/3,
   discrete/2,

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
-export_type([t/0]).

%% time stamp
-type(t() :: {integer(), integer(), integer()}).


%% number of sec to Unix Epoch
-define(UNX_EPOCH, 62167219200).
-define(BASE,          1000000).
-define(BASE3,            1000).


%%
%% time <=> microseconds
-spec(u/1 :: (t() | integer()) -> integer() | t()).

u({A2, A1, A0}) ->
   A0 + ?BASE * (A1 + ?BASE * A2);
u(X) 
 when is_integer(X) ->
   A0  = X rem ?BASE,
   Y   = X div ?BASE,
   A1  = Y rem ?BASE,
   A2  = Y div ?BASE,
   {A2, A1, A0}. 

%%
%% time <=> milliseconds
-spec(m/1 :: (t() | integer()) -> integer() | t()).

m({A2, A1, A0}) ->
   A0 div ?BASE3 + ?BASE3 * (A1 + ?BASE * A2);
m(X)
 when is_integer(X) ->
   A0  = X rem ?BASE3,
   Y   = X div ?BASE3,
   A1  = Y rem ?BASE,
   A2  = Y div ?BASE,
   {A2, A1, A0 * ?BASE3}. 


%%
%% time <=> seconds
-spec(s/1 :: (t() | integer()) -> integer() | t()).

s({A2, A1, _A0}) ->
   A1 + ?BASE * A2;
s(X)
 when is_integer(X) ->
   A0 = 0,
   A1  = X rem ?BASE,
   A2  = X div ?BASE,
   {A2, A1, A0}. 

%%
%% add time
-spec(add/2 :: (t(), t()) -> t()).

add({A2, A1, A0}, {B2, B1, B0}) ->
   T0 = B0 + A0,
   C0 = T0 rem ?BASE,
   Q0 = T0 div ?BASE,

   T1 = B1 + A1 + Q0,
   C1 = T1 rem ?BASE,
   Q1 = T1 div ?BASE,

   C2 = (B2 + A2 + Q1) rem ?BASE,
   {C2, C1, C0}.  

%%
%% subtract time
-spec(sub/2 :: (t(), t()) -> t()).

sub({A2, A1, A0}, {B2, B1, B0}) ->
   {C0, Q0} = sub(A0, B0, A1),
   {C1, Q1} = sub(Q0, B1, A2),
   {C2,  0} = sub(Q1, B2,  0),
   {C2, C1, C0}.
  
sub(X, Y, A)
 when X >=Y ->
   {X - Y, A};
sub(X, Y, A) ->
   {?BASE + X - Y, A - 1}.


%%
%% returns a sequence of times values on interval A, B
-spec(seq/3 :: (t(), t(), t()) -> [t()]).

seq({_, _, _}=A, {_, _, _}=B, {_, _, _}=C) ->
   seq_time(A, B, C);
seq(A, B, C)
 when is_integer(A), is_integer(B), is_integer(C) ->
   lists:seq(A, B, C).

seq_time(A, B, C)   
 when A =< B ->
   [A | seq_time(add(A, C), B, C)];
seq_time(_, _, _) ->
   []. 

%%
%% calculate discrete time 
-spec(discrete/2 :: (t(), t()) -> t()).

discrete({_, _, _}=X, {_, _, _}=Y) ->
   discrete(u(X), u(Y));

discrete(X, Y)
 when is_integer(X), is_integer(Y) ->
   u((X div Y) * Y).






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


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------


