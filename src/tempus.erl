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
%%   date/time utility for Erlang. It uses build-in time-stamp triplet as base
%%   type to carry on date-time.
%%    
%%
%% @todo
%%   * optimize arithmetic
%%   * how to handle ac / bc correctly
%%
-module(tempus).
-include("include/macro.hrl").

-export([
   %% time to micro-, milli-, second
   u/1,
   m/1,
   s/1,
   d/1,

   %% micro-, milli-, second to time
   t/2,

   %% diff
   diff/1,
   add/2,
   sub/2,
   mul/2,
   seq/3,
   discrete/2,

   % events
   event/2,
   timer/2,
   reset/2,
   cancel/1,

   % time convert utility
   iso8601/1
  ,decode/1
  ,decode/2
  ,encode/1
  ,encode/2
]).
-export_type([t/0, timer/0]).

%% data types
-type(t()        :: {integer(), integer(), integer()}).
-type(timer()    :: {timer | event, integer(), reference()}).

%% number of sec to Unix Epoch
-define(UNX_EPOCH, 62167219200).
-define(BASE,          1000000).
-define(BASE3,            1000).
-define(T0,  {{0,1,1},{0,0,0}}).

%%
%% time to micro-, milli-, second or date-time
-spec(u/1 :: (t()) -> integer()).
-spec(m/1 :: (t()) -> integer()).
-spec(s/1 :: (t()) -> integer()).
-spec(d/1 :: (t()) -> calendar:datetime()).

u({A2, A1, A0}) ->
   A0 + ?BASE * (A1 + ?BASE * A2);
u(X) 
 when is_integer(X) ->
   X.

m({A2, A1, A0}) ->
   A0 div ?BASE3 + ?BASE3 * (A1 + ?BASE * A2);
m(X)
 when is_integer(X) ->
   X.

s({A2, A1, _A0}) ->
   A1 + ?BASE * A2;
s(X)
 when is_integer(X) ->
   X.

d({A2, A1, _A0}) ->
   case A1 + ?BASE * A2 + ?UNX_EPOCH of
      Ts when Ts >= 0 ->
         calendar:gregorian_seconds_to_datetime(Ts);
      _ ->
         Sec = ?UNX_EPOCH - ?BASE * A2 - A1,
         {{Y, M, D},  T} = calendar:gregorian_seconds_to_datetime(Sec),
         {{-Y, M, D}, T}
   end;
d({{_, _, _}, {_, _, _}}=X) ->
   X.


%%
%% micro-, milli-, second to time
-spec(t/2 :: (u | m | s | d, integer()) -> t()).

t(u, X) 
 when is_integer(X) ->
   A0  = X rem ?BASE,
   Y   = X div ?BASE,
   A1  = Y rem ?BASE,
   A2  = Y div ?BASE,
   {A2, A1, A0};

t(m, X)
 when is_integer(X) ->
   A0  = X rem ?BASE3,
   Y   = X div ?BASE3,
   A1  = Y rem ?BASE,
   A2  = Y div ?BASE,
   {A2, A1, A0 * ?BASE3};

t(s, X)
 when is_integer(X) ->
   A0 = 0,
   A1  = X rem ?BASE,
   A2  = X div ?BASE,
   {A2, A1, A0};

t(_, {_,_,_}=X) ->
   X;

t(d, {{Y, _, _}, {_, _, _}}=Date)
 when Y >= 0 ->
   Sec = calendar:datetime_to_gregorian_seconds(Date) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0};
t(d, {{Y, M, D}, {_, _, _}=T})
 when Y  < 0 ->
   Sec = -1 * calendar:datetime_to_gregorian_seconds({{-1 * Y, M, D}, T}) - ?UNX_EPOCH,   
   {Sec div ?BASE, Sec rem ?BASE, 0};
t(d, X)
 when is_integer(X) ->
   t(s, X);
t(d, {_, _, _}=X) ->
   X.


%%
%% calculate time difference, return micro- seconds
-spec(diff/1 :: (t()) -> integer()).

diff(T) ->
   sub(os:timestamp(), T).

%%
%% add time
-spec(add/2 :: (t(), t()) -> t()).

add({_, _, _}=A, {_, _, _}=B) ->
   add_time(A, B);
add(A, B)
 when is_integer(A) orelse is_integer(B) ->
   add_time(tempus:t(s, A), tempus:t(s, B));
add(A, B)
 when is_integer(A), is_integer(B) ->
   A + B.

add_time({A2, A1, A0}, {B2, B1, B0}) ->
   {C0, Q0} = add_time(A0, B0,  0),
   {C1, Q1} = add_time(A1, B1, Q0),
   {C2,  _} = add_time(A2, B2, Q1),
   {C2, C1, C0}.
  
add_time(X, Y, Q) ->
   T = X + Y + Q,
   {T rem ?BASE, T div ?BASE}.   

%%
%% subtract time
-spec(sub/2 :: (t(), t()) -> t()).

sub({_, _, _}=A, {_, _, _}=B) ->
   sub_time(A, B);
sub(A, B)
 when is_integer(A) orelse is_integer(B) ->
   sub_time(tempus:t(s, A), tempus:t(s, B));
sub(A, B)
 when is_integer(A), is_integer(B) ->
   A - B.

sub_time({A2, A1, A0}, {B2, B1, B0}) ->
   {C0, Q0} = sub_time(A0, B0, A1),
   {C1, Q1} = sub_time(Q0, B1, A2),
   {C2,  0} = sub_time(Q1, B2,  0),
   {C2, C1, C0}.
  
sub_time(X, Y, A)
 when X >=Y ->
   {X - Y, A};
sub_time(X, Y, A) ->
   {?BASE + X - Y, A - 1}.

%%
%% multiply time
-spec(mul/2 :: (t(), t()) -> t()).

mul({_, _, _}=A, {_, _, _}=B) ->
   mul_time(A, B);
mul(A, B)
 when is_integer(A) orelse is_integer(B) ->
   mul_time(tempus:t(s, A), tempus:t(s, B));
mul(A, B)
 when is_integer(A), is_integer(B) ->
   A * B.

mul_time({A2, A1, A0}, {B2, B1, B0}) ->
   {C0, Q0} = mul_time(A0, B0,  0),
   {C1, Q1} = mul_time(A1, B1, Q0),
   {C2,  _} = mul_time(A2, B2, Q1),
   {C2, C1, C0}.

mul_time(X, Y, Q) ->
   T = (X * Y) + Q,
   {T rem ?BASE, T div ?BASE}.  

%%
%% returns a sequence of times values on interval A, B
-spec(seq/3 :: (t(), t(), t()) -> [t()]).

seq({_, _, _}=A, {_, _, _}=B, {_, _, _}=C) ->
   seq_time(A, B, C);
seq({_, _, _}=A, {_, _, _}=B, C)
 when is_integer(C) ->
   seq_time(A, B, t(s, C));   
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
   A = u(X),
   B = u(Y),
   t(u, (A div B) * B);

discrete({_, _, _}=X, Y)
 when is_integer(Y) ->
   A = u(X),
   B = Y * 1000000,
   t(u, (A div B) * B);

discrete(X, Y)
 when is_integer(X), is_integer(Y) ->
   (X div Y) * Y.


%%%------------------------------------------------------------------
%%%
%%% events
%%%
%%%------------------------------------------------------------------

%%
%% raise event after timeout
-spec(event/2 :: (integer() | timer(), any()) -> any()).

event(T, Msg)
 when is_integer(T) ->
   {event, T, erlang:send_after(T, self(), Msg)};

event({A, B, C}=T, Msg)
 when is_integer(A), is_integer(B), is_integer(C) ->
   event(m(T), Msg);

event({event, _, _}=T, _Msg) ->
   T;

event(T, _Msg) ->
   T.

%%
%% define timer
-spec(timer/2 :: (integer() | timer(), any()) -> timer()).

timer(T, Msg)
 when is_integer(T) ->
   {timer, T, erlang:send_after(T, self(), Msg)};

timer({T0, Drift}=T, Msg)
 when is_integer(T0) ->
   T1 = case erlang:trunc(T0 * Drift) of
      0 ->
         T0;
      X ->
         T0 + random:uniform(X)
   end,
   {timer, T, erlang:send_after(T1, self(), Msg)};

timer({A, B, C}=T, Msg)
 when is_integer(A), is_integer(B), is_integer(C) ->
   timer(m(T), Msg);

timer({{A, B, C}=T, Drift}, Msg)
 when is_integer(A), is_integer(B), is_integer(C) ->
   timer({m(T), Drift}, Msg);


timer({timer, _, _}=T, Msg) ->
   reset(T, Msg);

timer(T, _Msg) ->
   T.

%%
%% reset event's timeout 
-spec(reset/2 :: (integer() | timer(), any()) -> any()).

reset(T, Msg)
 when is_integer(T) ->
   tempus:event(T, Msg);

reset({event, _, _}=T, Msg) ->
   tempus:event(tempus:cancel(T), Msg);

reset({timer, _, _}=T, Msg) ->
   tempus:timer(tempus:cancel(T), Msg).

%%
%% cancel event
-spec(cancel/1 :: (any()) -> integer()).

cancel(T)
 when is_integer(T) ->
   T;

cancel({event, T, Timer}) ->
   erlang:cancel_timer(Timer),
   T;

cancel({timer, T, Timer}) ->
   erlang:cancel_timer(Timer),
   T;

cancel(T) ->
   T.


%%%------------------------------------------------------------------
%%%
%%% time convert
%%%
%%%------------------------------------------------------------------

%%
%%
iso8601(X)
 when is_binary(X) orelse is_list(X) ->
   decode_iso8601(scalar:s(X)).   

decode_iso8601(X) ->
   [Xd | T] = split(X, <<$T>>),
   [Xt | Z] = split(T, [<<$Z>>, <<$+>>, <<$->>]),
   Ts       = t(d, {decode_iso8601_date(Xd), decode_iso8601_time(Xt)}),
   case at(T, size(Xt)) of
      $Z -> 
         Ts;
      $+ ->
         sub(Ts, decode_iso8601_tz(Z));
      $- ->
         add(Ts, decode_iso8601_tz(Z))
   end.

%%
%%
split([], _) ->
   [<<>>];
split([X], Pat) ->
   split(X, Pat);
split(X, Pat)
 when is_binary(X) ->
   binary:split(X, Pat).

%%
%%
at([<<>>],  _) ->
   $Z;
at([X], I) ->
   at(X, I);
at(X,   I)
 when I < size(X) ->
   binary:at(X, I);
at(_,   _) ->
   $Z.


decode_iso8601_date(<<$-, Y:4/binary, $-, M:2/binary, $-, D:2/binary>>) ->
   {-1 * scalar:i(Y), scalar:i(M), scalar:i(D)};
decode_iso8601_date(<<$-, Y:4/binary, M:2/binary, D:2/binary>>) ->
   {-1 * scalar:i(Y), scalar:i(M), scalar:i(D)};
decode_iso8601_date(<<Y:4/binary, $-, M:2/binary, $-, D:2/binary>>) ->
   {scalar:i(Y), scalar:i(M), scalar:i(D)};
decode_iso8601_date(<<Y:4/binary, M:2/binary, D:2/binary>>) ->
   {scalar:i(Y), scalar:i(M), scalar:i(D)};
decode_iso8601_date(<<>>) ->
   {0, 1, 1}.

decode_iso8601_time(<<H:2/binary, $:, M:2/binary, $:, S:2/binary, $., _/binary>>) ->
   {scalar:i(H), scalar:i(M), scalar:i(S)};
decode_iso8601_time(<<H:2/binary, $:, M:2/binary, $:, S:2/binary>>) ->
   {scalar:i(H), scalar:i(M), scalar:i(S)};
decode_iso8601_time(<<H:2/binary, $:, M:2/binary>>) ->
   {scalar:i(H), scalar:i(M), 0};

decode_iso8601_time(<<H:2/binary, M:2/binary, S:2/binary, $., _/binary>>) ->
   {scalar:i(H), scalar:i(M), scalar:i(S)};
decode_iso8601_time(<<H:2/binary, M:2/binary, S:2/binary>>) ->
   {scalar:i(H), scalar:i(M), scalar:i(S)};
decode_iso8601_time(<<H:2/binary, M:2/binary>>) ->
   {scalar:i(H), scalar:i(M), 0};

decode_iso8601_time(<<H:2/binary>>) ->
   {scalar:i(H), 0, 0};

decode_iso8601_time(<<>>) ->
   {0, 0, 0}.

decode_iso8601_tz([X]) ->
   decode_iso8601_tz(X);
decode_iso8601_tz(<<$Z>>) ->
   0;
decode_iso8601_tz(<<>>) ->
   0;
decode_iso8601_tz(<<H:2/binary, $:, M:2/binary>>) ->
   scalar:i(H) * 3600 + scalar:i(M) * 60;
decode_iso8601_tz(<<H:2/binary,     M:2/binary>>) ->
   scalar:i(H) * 3600 + scalar:i(M) * 60;
decode_iso8601_tz(<<H:2/binary>>) ->
   scalar:i(H) * 3600.

%%
%% parses literal date time
-spec(decode/1 :: (any()) -> tempus:t()).
-spec(decode/2 :: (list(), any()) -> tempus:t()).

decode(Val) ->
   decode(?ISO8601, Val).
decode(Fmt, Val) ->
   decode(scalar:c(Fmt), scalar:c(Val), {{0,1,1},{0,0,0}}).

%% Day 
%% %a  An abbreviated textual representation of the day Sun through Sat
%% %A  A full textual representation of the day  Sunday through Saturday
%% %d  Two-digit day of the month (with leading zeros) 01 to 31
%% %e  Day of the month, with a space preceding single digits. 1 to 31
%% %j  Day of the year, 3 digits with leading zeros  001 to 366
%% %u  ISO-8601 numeric representation of the day of the week  1 (for Monday) though 7 (for Sunday)
%% %w  Numeric representation of the day of the week 0 (for Sunday) through 6 (for Saturday)
decode([$%, $a | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {_, Rest} = day_of_week(Val),
   decode(Tail, Rest, Acc);

decode([$%, $A | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {_, Rest} = full_day_of_week(Val),
   decode(Tail, Rest, Acc);

decode([$%, $d | Tail], Val, {{Y,M,_D}, {_H,_N,_S}=Time}) ->
   {Day, Rest} = lists:split(2, Val),
   decode(Tail, Rest, {{Y,M,scalar:i(Day)}, Time});

decode([$%, $e | Tail], Val, {{Y,M,_D}, {_H,_N,_S}=Time}) ->
   {Day, Rest} = case lists:split(2, Val) of
      {[32 | V], R} -> {V, R};
      Result         -> Result
   end,
   decode(Tail, Rest, {{Y,M,scalar:i(Day)}, Time});

decode([$%, $j | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   % @todo: implement
   {Day, Rest} = lists:split(3, Val),
   _ = list_to_integer(Day),
   decode(Tail, Rest, Acc);

decode([$%, $u | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {Day, Rest} = lists:split(1, Val),
   _ = list_to_integer(Day),
   decode(Tail, Rest, Acc);

decode([$%, $w | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {Day, Rest} = lists:split(1, Val),
   _ = list_to_integer(Day),
   decode(Tail, Rest, Acc);

%%
%% Month
%% %b  Abbreviated month name, based on the locale Jan through Dec
%% %B  Full month name, based on the locale  January through December
%% %h  Abbreviated month name, based on the locale (an alias of %b)  Jan through Dec
%% %m  Two digit representation of the month 01 (for January) through 12 (for December)
decode([$%, $b | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {M, Rest} = month_of_year(Val),
   decode(Tail, Rest, {{Y,M,D}, Time});

decode([$%, $B | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {M, Rest} = full_month_of_year(Val),
   decode(Tail, Rest, {{Y,M,D}, Time});

decode([$%, $m | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {Month, Rest} = lists:split(2, Val),
   decode(Tail, Rest, {{Y,scalar:i(Month),D}, Time});

%%
%% Year
%% %C  Two digit representation of the century (year divided by 100, truncated to an integer)  19 for the 20th Century
%% %g  Two digit representation of the year going by ISO-8601:1988 standards (see %V)  Example: 09 for the week of January 6, 2009
%% %G  The full four-digit version of %g Example: 2008 for the week of January 3, 2009
%% %y  Two digit representation of the year  Example: 09 for 2009, 79 for 1979
%% %Y  Four digit representation for the year  Example: 2038
decode([$%, $y | Tail], Val, {{_Y,M,D}, {_H,_N,_S}=Time}) ->
   {Year, Rest} = lists:split(2, Val),
   Y = 2000 + scalar:i(Year), 
   decode(Tail, Rest, {{Y,M,D}, Time});

decode([$%, $Y | Tail], [$- | Val], {{_Y,M,D}, {_H,_N,_S}=Time}) ->
   %% BC Year (e.g. -0063-09-23)
   {Year, Rest} = lists:split(4, Val),
   decode(Tail, Rest, {{-1 * scalar:i(Year),M,D}, Time});

decode([$%, $Y | Tail], Val, {{_Y,M,D}, {_H,_N,_S}=Time}) ->
   {Year, Rest} = lists:split(4, Val),
   decode(Tail, Rest, {{scalar:i(Year),M,D}, Time});


%% 
%% Time
%% %H  Two digit representation of the hour in 24-hour format  00 through 23
%% %I  Two digit representation of the hour in 12-hour format  01 through 12
%% %l (lower-case 'L') Hour in 12-hour format, with a space preceeding single digits  1 through 12
%% %M  Two digit representation of the minute  00 through 59
%% %p  UPPER-CASE 'AM' or 'PM' based on the given time Example: AM for 00:31, PM for 22:23
%% %P  lower-case 'am' or 'pm' based on the given time Example: am for 00:31, pm for 22:23
%% %r  Same as "%I:%M:%S %p" Example: 09:34:17 PM for 21:34:17
%% %R  Same as "%H:%M" Example: 00:35 for 12:35 AM, 16:44 for 4:44 PM
%% %S  Two digit representation of the second  00 through 59
%% %f  Microsecond as a decimal number, zero-padded on the left. 000000, 000001, ..., 999999
%% %T  Same as "%H:%M:%S"  Example: 21:34:17 for 09:34:17 PM
%% %X  Preferred time representation based on locale, without the date Example: 03:59:16 or 15:59:16
%% %z  Either the time zone offset from UTC or the abbreviation (depends on operating system)  Example: -0500 or EST for Eastern Time
%% %Z  The time zone offset/abbreviation option NOT given by %z (depends on operating system)  Example: -0500 or EST for Eastern Time
decode([$%, $H | Tail], Val, {{_Y,_M,_D}=Date, {_H,N,S}}) ->
   {Hour, Rest} = lists:split(2, Val),
   decode(Tail, Rest, {Date, {scalar:i(Hour), N, S}});

decode([$%, $M | Tail], Val, {{_Y,_M,_D}=Date, {H,_N,S}}) ->
   {Min, Rest} = lists:split(2, Val),
   decode(Tail, Rest, {Date, {H, scalar:i(Min), S}});

decode([$%, $S | Tail], Val, {{_Y,_M,_D}=Date, {H,N,_S}}) ->
   {Sec, Rest} = lists:split(2, Val),
   decode(Tail, Rest, {Date, {H, N, scalar:i(Sec)}});

decode([$%, $f | _] = Fmt, [C | Val], Acc)
 when C >= $0, C =< $9 ->
   decode(Fmt, Val, Acc);
decode([$%, $f | Tail], Val, Acc) ->
   decode(Tail, Val, Acc);


%% Time and Date Stamps
%% %c  Preferred date and time stamp based on local  Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
%% %D  Same as "%m/%d/%y"  Example: 02/05/09 for February 5, 2009
%% %F  Same as "%Y-%m-%d" (commonly used in database datestamps) Example: 2009-02-05 for February 5, 2009
%% %s  Unix Epoch Time timestamp (same as the time() function) Example: 305815200 for September 10, 1979 08:40:00 AM
%% %x  Preferred date representation based on locale, without the time Example: 02/05/09 for February 5, 2009
decode([$%, $s | _Tail], Val, _Acc) ->
   {Sec, _Rest} = case string:chr(Val, 32) of
      0 -> {Val, []};
      I -> lists:split(I - 1, Val)
   end,
   Epoch = scalar:i(Sec),
   {Epoch div ?BASE, Epoch rem ?BASE, 0};

decode([H | Tail], [H | Val], Acc) ->
   decode(Tail, Val, Acc);

decode([], _Val, {{Y, _, _}, _}=Acc)
 when Y >= 0 ->
   Sec = calendar:datetime_to_gregorian_seconds(Acc) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0};
decode([], _Val, {{Y,M,D}, T}) ->
   Sec = -1 * calendar:datetime_to_gregorian_seconds({{-1 * Y,M,D}, T}) - ?UNX_EPOCH,   
   {Sec div ?BASE, Sec rem ?BASE, 0}.


day_of_week("Mon" ++ Tail) -> {1, Tail};
day_of_week("Tue" ++ Tail) -> {2, Tail};
day_of_week("Wed" ++ Tail) -> {3, Tail};
day_of_week("Thu" ++ Tail) -> {4, Tail};
day_of_week("Fri" ++ Tail) -> {5, Tail};
day_of_week("Sat" ++ Tail) -> {6, Tail};
day_of_week("Sun" ++ Tail) -> {7, Tail};

day_of_week(1) -> "Mon";
day_of_week(2) -> "Tue";
day_of_week(3) -> "Wed";
day_of_week(4) -> "Thu";
day_of_week(5) -> "Fri";
day_of_week(6) -> "Sat";
day_of_week(7) -> "Sun".


full_day_of_week("Monday" ++ Tail)    -> {1, Tail};
full_day_of_week("Tuesday" ++ Tail)   -> {2, Tail};
full_day_of_week("Wednesday" ++ Tail) -> {3, Tail};
full_day_of_week("Thursday" ++ Tail)  -> {4, Tail};
full_day_of_week("Friday" ++ Tail)    -> {5, Tail};
full_day_of_week("Saturday" ++ Tail)  -> {6, Tail};
full_day_of_week("Sunday" ++ Tail)    -> {7, Tail}.

month_of_year("Jan" ++ Tail) -> {1, Tail};
month_of_year("Feb" ++ Tail) -> {2, Tail};
month_of_year("Mar" ++ Tail) -> {3, Tail};
month_of_year("Apr" ++ Tail) -> {4, Tail};
month_of_year("May" ++ Tail) -> {5, Tail};
month_of_year("Jun" ++ Tail) -> {6, Tail};
month_of_year("Jul" ++ Tail) -> {7, Tail};
month_of_year("Aug" ++ Tail) -> {8, Tail};
month_of_year("Sep" ++ Tail) -> {9, Tail};
month_of_year("Oct" ++ Tail) -> {10, Tail};
month_of_year("Nov" ++ Tail) -> {11, Tail};
month_of_year("Dec" ++ Tail) -> {12, Tail};
month_of_year(1) -> "Jan";
month_of_year(2) -> "Feb";
month_of_year(3) -> "Mar";
month_of_year(4) -> "Apr";
month_of_year(5) -> "May";
month_of_year(6) -> "Jun";
month_of_year(7) -> "Jul";
month_of_year(8) -> "Aug";
month_of_year(9) -> "Sep";
month_of_year(10) -> "Oct";
month_of_year(11) -> "Nov";
month_of_year(12) -> "Dec".


full_month_of_year("January"  ++ Tail) -> {1, Tail};
full_month_of_year("February" ++ Tail) -> {2, Tail};
full_month_of_year("March" ++ Tail)    -> {3, Tail};
full_month_of_year("April" ++ Tail)    -> {4, Tail};
full_month_of_year("May"  ++ Tail)     -> {5, Tail};
full_month_of_year("June" ++ Tail)     -> {6, Tail};
full_month_of_year("July" ++ Tail)     -> {7, Tail};
full_month_of_year("August" ++ Tail)   -> {8, Tail};
full_month_of_year("September" ++ Tail)-> {9, Tail};
full_month_of_year("October"  ++ Tail) -> {10, Tail};
full_month_of_year("November" ++ Tail) -> {11, Tail};
full_month_of_year("December" ++ Tail) -> {12, Tail}.


%%
%% encode date time
-spec(encode/1 :: (tempus:t()) -> list()).
-spec(encode/2 :: (list(), tempus:t()) -> list()).

encode(Val) ->
   encode(?ISO8601, Val).

encode(Fmt, Time)
 when is_binary(Fmt) ->
   encode(binary_to_list(Fmt), Time);
   
encode(Fmt, {{_Y,_M,_D}, {_H,_N,_S}}=Val) ->
   encode(Fmt, Val, []);

encode(Fmt, {Mega, Sec, _Micro}) ->
   encode(Fmt, Mega * ?BASE + Sec);

encode(Fmt, Val) 
 when is_integer(Val), Val > 0 ->
   encode(Fmt, calendar:gregorian_seconds_to_datetime(Val + ?UNX_EPOCH), []);

encode(Fmt, Val)
 when is_integer(Val) ->
   case Val + ?UNX_EPOCH of
      X when X > 0 ->
         encode(Fmt, calendar:gregorian_seconds_to_datetime(X));
      _ ->
         "-" ++ encode(Fmt, calendar:gregorian_seconds_to_datetime(-1 * Val + ?UNX_EPOCH), [])
   end.



% decode([], _Val, {{Y, _, _}, _}=Acc)
%  when Y > 0 ->
%    Sec = calendar:datetime_to_gregorian_seconds(Acc) - ?UNX_EPOCH,
%    {Sec div ?BASE, Sec rem ?BASE, 0};
% decode([], _Val, {{Y,M,D}, T}) ->
%    Sec = -1 * calendar:datetime_to_gregorian_seconds({{-1 * Y,M,D}, T}) - ?UNX_EPOCH,   
%    {Sec div ?BASE, Sec rem ?BASE, 0}.



%% Day 
%% %a  An abbreviated textual representation of the day Sun through Sat
%% %A  A full textual representation of the day  Sunday through Saturday
%% %d  Two-digit day of the month (with leading zeros) 01 to 31
%% %e  Day of the month, with a space preceding single digits. 1 to 31
%% %j  Day of the year, 3 digits with leading zeros  001 to 366
%% %u  ISO-8601 numeric representation of the day of the week  1 (for Monday) though 7 (for Sunday)
%% %w  Numeric representation of the day of the week 0 (for Sunday) through 6 (for Saturday)
encode([$%, $a | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   encode(Tail, Val, [day_of_week(Day) | Acc]);  

encode([$%, $A | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   encode(Tail, Val, [fday_of_week(Day) | Acc]);  

encode([$%, $d | Tail], {{_Y,_M,D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [D]) | Acc]);

encode([$%, $e | Tail], {{_Y,_M,D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10. B", [D]) | Acc]);

encode([$%, $j | Tail], {{Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Y-1, 12, 31),
   encode(Tail, Val, [io_lib:format("~3.10.0B", [Day]) | Acc]);

encode([$%, $u | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   encode(Tail, Val, [io_lib:format("~B", [Day]) | Acc]);

encode([$%, $w | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = case calendar:day_of_the_week(Date) of
      7 -> 0;
      D -> D
   end,
   encode(Tail, Val, [io_lib:format("~B", [Day]) | Acc]);

%%
%% Week
%% %U  Week number of the given year, starting with the first Sunday as the first week 13 (for the 13th full week of the year)
%% %V  ISO-8601:1988 week number of the given year, starting with the first week of the year with at least 4 weekdays, with Monday being the start of the week 01 through 53 (where 53 accounts for an overlapping week)
%% %W  A numeric representation of the week of the year, starting with the first Monday as the first week  46 (for the 46th week of the year beginning with a Monday)
%% Not supported

%%
%% Month
%% %b  Abbreviated month name, based on the locale Jan through Dec
%% %B  Full month name, based on the locale  January through December
%% %h  Abbreviated month name, based on the locale (an alias of %b)  Jan through Dec
%% %m  Two digit representation of the month 01 (for January) through 12 (for December)
encode([$%, $b | Tail], {{_Y, M,_D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [month_of_year(M) | Acc]);

encode([$%, $B | Tail], {{_Y, M,_D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [fmonth_of_year(M) | Acc]);

encode([$%, $m | Tail], {{_Y,M,_D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [M]) | Acc]);

%%
%% Year
%% %C  Two digit representation of the century (year divided by 100, truncated to an integer)  19 for the 20th Century
%% %g  Two digit representation of the year going by ISO-8601:1988 standards (see %V)  Example: 09 for the week of January 6, 2009
%% %G  The full four-digit version of %g Example: 2008 for the week of January 3, 2009
%% %y  Two digit representation of the year  Example: 09 for 2009, 79 for 1979
%% %Y  Four digit representation for the year  Example: 2038

encode([$%, $y | Tail], {{Y, _M,_D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [Y rem 1000])  | Acc]);

encode([$%, $Y | Tail], {{Y, _M,_D}, {_H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~4.10.0B", [Y])  | Acc]);

%% 
%% Time
%% %H  Two digit representation of the hour in 24-hour format  00 through 23
%% %I  Two digit representation of the hour in 12-hour format  01 through 12
%% %l (lower-case 'L') Hour in 12-hour format, with a space preceeding single digits  1 through 12
%% %M  Two digit representation of the minute  00 through 59
%% %p  UPPER-CASE 'AM' or 'PM' based on the given time Example: AM for 00:31, PM for 22:23
%% %P  lower-case 'am' or 'pm' based on the given time Example: am for 00:31, pm for 22:23
%% %r  Same as "%I:%M:%S %p" Example: 09:34:17 PM for 21:34:17
%% %R  Same as "%H:%M" Example: 00:35 for 12:35 AM, 16:44 for 4:44 PM
%% %S  Two digit representation of the second  00 through 59
%% %T  Same as "%H:%M:%S"  Example: 21:34:17 for 09:34:17 PM
%% %X  Preferred time representation based on locale, without the date Example: 03:59:16 or 15:59:16
%% %z  Either the time zone offset from UTC or the abbreviation (depends on operating system)  Example: -0500 or EST for Eastern Time
%% %Z  The time zone offset/abbreviation option NOT given by %z (depends on operating system)  Example: -0500 or EST for Eastern Time

encode([$%, $H | Tail], {{_Y, _M,_D}, {H,_N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [H])  | Acc]);

encode([$%, $M | Tail], {{_Y, _M,_D}, {_H,N,_S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [N])  | Acc]);

encode([$%, $S | Tail], {{_Y, _M,_D}, {_H,_N,S}}=Val, Acc) ->
   encode(Tail, Val, [io_lib:format("~2.10.0B", [S])  | Acc]);

%% Time and Date Stamps
%% %c  Preferred date and time stamp based on local  Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
%% %D  Same as "%m/%d/%y"  Example: 02/05/09 for February 5, 2009
%% %F  Same as "%Y-%m-%d" (commonly used in database datestamps) Example: 2009-02-05 for February 5, 2009
%% %s  Unix Epoch Time timestamp (same as the time() function) Example: 305815200 for September 10, 1979 08:40:00 AM
%% %x  Preferred date representation based on locale, without the time Example: 02/05/09 for February 5, 2009
encode([$%, $s | Tail], {{_Y,_M,_D}, {_H,_N,_S}}=Val, Acc) ->
   Sec = calendar:datetime_to_gregorian_seconds(Val) - ?UNX_EPOCH,
   encode(Tail, Val, [io_lib:format("~B", [Sec]) | Acc]);

%%
%% Miscellaneous
%% %n  A newline character ("\n")  ---
%% %t  A Tab character ("\t")
encode([H | Tail], Val, Acc) ->
   encode(Tail, Val, [H | Acc]);

encode([], _Val, Acc) ->
   lists:flatten(lists:reverse(Acc)).


%%-------------------------------------------------------------------
%%
%% private
%%
%%-------------------------------------------------------------------

%%
%% day of week

%%
%% full day of week
fday_of_week(1) -> "Monday";
fday_of_week(2) -> "Tuesday";
fday_of_week(3) -> "Wednesday";
fday_of_week(4) -> "Thursday";
fday_of_week(5) -> "Friday";
fday_of_week(6) -> "Saturday";
fday_of_week(7) -> "Sunday".


%%
%% month of year


%%
%% full month of year
fmonth_of_year(1) -> "January";
fmonth_of_year(2) -> "February";
fmonth_of_year(3) -> "March";
fmonth_of_year(4) -> "April";
fmonth_of_year(5) -> "May";
fmonth_of_year(6) -> "June";
fmonth_of_year(7) -> "July";
fmonth_of_year(8) -> "August";
fmonth_of_year(9) -> "September";
fmonth_of_year(10) -> "October";
fmonth_of_year(11) -> "November";
fmonth_of_year(12) -> "December".

