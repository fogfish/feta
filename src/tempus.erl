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
-include("include/macro.hrl").

-export([
   %% time to micro-, milli-, second
   u/1,
   m/1,
   s/1,

   %% micro-, milli-, second to time
   t/2,

   %% diff
   diff/1,
   add/2,
   sub/2,
   seq/3,
   discrete/2,

   % events
   event/2,
   reset/2,
   cancel/1,

   % time convert utility
   decode/1
  ,decode/2

   % now/0, 
   % sec/0, 
   % sec/1, 
   % milli/0, 
   % milli/1, 
   % micro/0, 
   % micro/1,

   % % time transform utility
   % inc/1, 
   % inc/2, 
   % dec/1, 
   % dec/2

]).
-export_type([t/0]).

%% time stamp
-type(t() :: {integer(), integer(), integer()}).


%% number of sec to Unix Epoch
-define(UNX_EPOCH, 62167219200).
-define(BASE,          1000000).
-define(BASE3,            1000).


%%
%% time to micro-, milli-, second
-spec(u/1 :: (t()) -> integer()).
-spec(m/1 :: (t()) -> integer()).
-spec(s/1 :: (t()) -> integer()).

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


%%
%% micro-, milli-, second to time
-spec(t/2 :: (u | m |s, integer()) -> t()).

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
   {A2, A1, A0}. 

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
%%% time convert
%%%
%%%------------------------------------------------------------------

%%
%% parses literal date time
-spec(decode/1 :: (any()) -> tempus:t()).
-spec(decode/2 :: (list(), any()) -> tempus:t()).

decode(Val) ->
   decode(?ISO8601, Val).
decode(Fmt, Val) ->
   decode(scalar:c(Fmt), scalar:c(Val), {{0,0,0},{0,0,0}}).

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

%% Time and Date Stamps
%% %c  Preferred date and time stamp based on local  Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
%% %D  Same as "%m/%d/%y"  Example: 02/05/09 for February 5, 2009
%% %F  Same as "%Y-%m-%d" (commonly used in database datestamps) Example: 2009-02-05 for February 5, 2009
%% %s  Unix Epoch Time timestamp (same as the time() function) Example: 305815200 for September 10, 1979 08:40:00 AM
%% %x  Preferred date representation based on locale, without the time Example: 02/05/09 for February 5, 2009
decode([$%, $s | Tail], Val, _Acc) ->
   {Sec, Rest} = case string:chr(Val, 32) of
      0 -> {Val, []};
      I -> lists:split(I - 1, Val)
   end,
   Epoch = scalar:i(Sec),
   {Epoch div ?BASE, Epoch rem ?BASE, 0};

decode([H | Tail], [H | Val], Acc) ->
   decode(Tail, Val, Acc);

decode([], _Val, Acc) ->
   Sec = calendar:datetime_to_gregorian_seconds(Acc) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0}.


day_of_week("Mon" ++ Tail) -> {1, Tail};
day_of_week("Tue" ++ Tail) -> {2, Tail};
day_of_week("Wed" ++ Tail) -> {3, Tail};
day_of_week("Thu" ++ Tail) -> {4, Tail};
day_of_week("Fri" ++ Tail) -> {5, Tail};
day_of_week("Sat" ++ Tail) -> {6, Tail};
day_of_week("Sun" ++ Tail) -> {7, Tail}.

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
month_of_year("Dec" ++ Tail) -> {12, Tail}.

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









% %%
% %% current time in seconds
% now() -> 
%    sec().

% %%
% %% current time in seconds
% sec() -> 
%    sec(os:timestamp()).

% %%
% %% convert time to second
% sec({Msec, Sec, _Usec}) ->
%    % erlang:now
%    Msec * 1000000 + Sec;
% sec({{_,_,_},{_,_,_}}=Date) ->
%    % erlang date/time
%    calendar:datetime_to_gregorian_seconds(Date) - ?UNX_EPOCH;
% sec(Sec)
%  when is_list(Sec), length(Sec) =:= 8 ->
%    % any 8-length time is considered to be YYYYMMDD format
%    % 1970 - 1974 dates has to be requested as iso8601 or use scalar utility
%    sec(parser:iso8601(Sec));
% sec(T)
%  when is_list(T) ->
%    case string:chr(T, $T) of
%       0 -> 
%          case scalar:decode(T) of
%             X when is_list(X) -> sec(list_to_existing_atom(X));
%             X -> sec(X)
%          end;
%       _ -> sec(parser:iso8601(T))
%    end;
% sec(T)
%  when is_binary(T) ->
%    sec(binary_to_list(T));
% sec(today) ->
%    86400;
% sec(day)   ->
%    86400;
% sec(week)  -> 
%    7  * 86400;
% sec(month) -> 
%    30 * 86400;
% sec(year)  -> 
%    365 * 86400;
% sec(hour)  ->
%     3600;
% sec(Sec)
%  when is_float(Sec) ->
%    sec(erlang:round(Sec));
% sec(Sec)
%  when is_integer(Sec), Sec < 9999999999 ->
%    Sec; 
% sec(Milli)
%  when is_integer(Milli) ->
%    % given time value exceeds max allowed Sec value
%    Milli div 1000.

% %%
% %% current time in milliseconds   
% milli() ->
%    milli(os:timestamp()).

% milli({Msec, Sec, Usec}) ->
%    (Msec * 1000000 + Sec) * 1000 + Usec div 1000.

% %%
% %% current time in microseconds
% micro() ->
%    micro(os:timestamp()).

% micro({Msec, Sec, Usec}) ->
%    (Msec * 1000000 + Sec) * 1000000 + Usec.

% %%%------------------------------------------------------------------
% %%%
% %%% transform
% %%%
% %%%------------------------------------------------------------------

% %%
% %% increase time by T seconds
% inc(T) ->
%    inc(os:timestamp(), sec(T)).

% inc({Msec, Sec, Usec}, T)
%  when is_integer(T) ->
%    case Sec + T of
%       X when X =< 1000000 ->
%          {Msec, X, Usec};
%       X ->
%          {Msec + (X div 1000000), X rem 1000000, Usec}
%    end.

% %%
% %% increase time by T seconds
% dec(T) ->
%    dec(os:timestamp(), sec(T)).

% dec({Msec, Sec, Usec}, T)
%  when is_integer(T) ->
%    case Sec - T of
%       X when X >= 0 ->
%          {Msec, X, Usec};
%       X ->
%          {Msec - 1, 1000000 + X, Usec}
%    end.


% %%%------------------------------------------------------------------
% %%%
% %%% private
% %%%
% %%%------------------------------------------------------------------


