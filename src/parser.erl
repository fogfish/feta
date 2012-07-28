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
%%
-module(parser).

-export([datetime/2]).
-define(UNX_EPOCH, 62167219200).

%%%----------------------------------------------------------------------------   
%%%
%%% date/time parser 
%%%
%%%----------------------------------------------------------------------------   

datetime(Fmt, Val) when is_binary(Fmt) ->
   datetime(binary_to_list(Fmt), Val);

datetime(Fmt, Val) when is_binary(Val) ->
   datetime(Fmt, binary_to_list(Val));
   
datetime(Fmt, Val) ->
   datetime(Fmt, Val, {{0,0,0},{0,0,0}}).

%% Day 
%% %a  An abbreviated textual representation of the day Sun through Sat
%% %A  A full textual representation of the day  Sunday through Saturday
%% %d  Two-digit day of the month (with leading zeros) 01 to 31
%% %e  Day of the month, with a space preceding single digits. 1 to 31
%% %j  Day of the year, 3 digits with leading zeros  001 to 366
%% %u  ISO-8601 numeric representation of the day of the week  1 (for Monday) though 7 (for Sunday)
%% %w  Numeric representation of the day of the week 0 (for Sunday) through 6 (for Saturday)
datetime([$%, $a | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {_, Rest} = day_of_week(Val),
   datetime(Tail, Rest, Acc);

datetime([$%, $A | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {_, Rest} = fday_of_week(Val),
   datetime(Tail, Rest, Acc);

datetime([$%, $d | Tail], Val, {{Y,M,_D}, {_H,_N,_S}=Time}) ->
   {Day, Rest} = lists:split(2, Val),
   D = list_to_integer(Day),
   datetime(Tail, Rest, {{Y,M,D}, Time});

datetime([$%, $e | Tail], Val, {{Y,M,_D}, {_H,_N,_S}=Time}) ->
   {Day, Rest} = case lists:split(2, Val) of
   	{[32 | V], R} -> {V, R};
   	Result         -> Result
   end,
   D = list_to_integer(Day),
   datetime(Tail, Rest, {{Y,M,D}, Time});

datetime([$%, $j | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {Day, Rest} = lists:split(3, Val),
   _ = list_to_integer(Day),
   datetime(Tail, Rest, Acc);

datetime([$%, $u | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {Day, Rest} = lists:split(1, Val),
   _ = list_to_integer(Day),
   datetime(Tail, Rest, Acc);

datetime([$%, $w | Tail], Val, {{_Y,_M,_D}, {_H,_N,_S}}=Acc) ->
   {Day, Rest} = lists:split(1, Val),
   _ = list_to_integer(Day),
   datetime(Tail, Rest, Acc);

%%
%% Month
%% %b  Abbreviated month name, based on the locale Jan through Dec
%% %B  Full month name, based on the locale  January through December
%% %h  Abbreviated month name, based on the locale (an alias of %b)  Jan through Dec
%% %m  Two digit representation of the month 01 (for January) through 12 (for December)
datetime([$%, $b | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {M, Rest} = month_of_year(Val),
   datetime(Tail, Rest, {{Y,M,D}, Time});

datetime([$%, $B | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {M, Rest} = fmonth_of_year(Val),
   datetime(Tail, Rest, {{Y,M,D}, Time});

datetime([$%, $m | Tail], Val, {{Y,_M,D}, {_H,_N,_S}=Time}) ->
   {Month, Rest} = lists:split(2, Val),
   M = list_to_integer(Month),
   datetime(Tail, Rest, {{Y,M,D}, Time});

%%
%% Year
%% %C  Two digit representation of the century (year divided by 100, truncated to an integer)  19 for the 20th Century
%% %g  Two digit representation of the year going by ISO-8601:1988 standards (see %V)  Example: 09 for the week of January 6, 2009
%% %G  The full four-digit version of %g Example: 2008 for the week of January 3, 2009
%% %y  Two digit representation of the year  Example: 09 for 2009, 79 for 1979
%% %Y  Four digit representation for the year  Example: 2038
datetime([$%, $y | Tail], Val, {{_Y,M,D}, {_H,_N,_S}=Time}) ->
   {Year, Rest} = lists:split(2, Val),
   Y = 2000 + list_to_integer(Year), % FIXME:
   datetime(Tail, Rest, {{Y,M,D}, Time});

datetime([$%, $Y | Tail], Val, {{_Y,M,D}, {_H,_N,_S}=Time}) ->
   {Year, Rest} = lists:split(4, Val),
   Y = list_to_integer(Year),
   datetime(Tail, Rest, {{Y,M,D}, Time});


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
datetime([$%, $H | Tail], Val, {{_Y,_M,_D}=Date, {_H,N,S}}) ->
   {Hour, Rest} = lists:split(2, Val),
   H = list_to_integer(Hour), 
   datetime(Tail, Rest, {Date, {H, N, S}});

datetime([$%, $M | Tail], Val, {{_Y,_M,_D}=Date, {H,_N,S}}) ->
   {Min, Rest} = lists:split(2, Val),
   N = list_to_integer(Min), 
   datetime(Tail, Rest, {Date, {H, N, S}});

datetime([$%, $S | Tail], Val, {{_Y,_M,_D}=Date, {H,N,_S}}) ->
   {Sec, Rest} = lists:split(2, Val),
   S = list_to_integer(Sec), 
   datetime(Tail, Rest, {Date, {H, N, S}});

%% Time and Date Stamps
%% %c  Preferred date and time stamp based on local  Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
%% %D  Same as "%m/%d/%y"  Example: 02/05/09 for February 5, 2009
%% %F  Same as "%Y-%m-%d" (commonly used in database datestamps) Example: 2009-02-05 for February 5, 2009
%% %s  Unix Epoch Time timestamp (same as the time() function) Example: 305815200 for September 10, 1979 08:40:00 AM
%% %x  Preferred date representation based on locale, without the time Example: 02/05/09 for February 5, 2009
datetime([$%, $s | Tail], Val, _Acc) ->
   {Sec, Rest} = case string:chr(Val, 32) of
   	0 -> {Val, []};
   	I -> lists:split(I - 1, Val)
   end,
   S = list_to_integer(Sec), 
   datetime(Tail, Rest, calendar:gregorian_seconds_to_datetime(S + ?UNX_EPOCH));

datetime([H | Tail], [H | Val], Acc) ->
   datetime(Tail, Val, Acc);

datetime([], _Val, Acc) ->
   Acc.


day_of_week("Mon" ++ Tail) -> {1, Tail};
day_of_week("Tue" ++ Tail) -> {2, Tail};
day_of_week("Wed" ++ Tail) -> {3, Tail};
day_of_week("Thu" ++ Tail) -> {4, Tail};
day_of_week("Fri" ++ Tail) -> {5, Tail};
day_of_week("Sat" ++ Tail) -> {6, Tail};
day_of_week("Sun" ++ Tail) -> {7, Tail}.

fday_of_week("Monday" ++ Tail)    -> {1, Tail};
fday_of_week("Tuesday" ++ Tail)   -> {2, Tail};
fday_of_week("Wednesday" ++ Tail) -> {3, Tail};
fday_of_week("Thursday" ++ Tail)  -> {4, Tail};
fday_of_week("Friday" ++ Tail)    -> {5, Tail};
fday_of_week("Saturday" ++ Tail)  -> {6, Tail};
fday_of_week("Sunday" ++ Tail)    -> {7, Tail}.

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

fmonth_of_year("January" ++ Tail)  -> {1, Tail};
fmonth_of_year("February" ++ Tail) -> {2, Tail};
fmonth_of_year("March" ++ Tail)    -> {3, Tail};
fmonth_of_year("April" ++ Tail)    -> {4, Tail};
fmonth_of_year("May" ++ Tail)      -> {5, Tail};
fmonth_of_year("June" ++ Tail)     -> {6, Tail};
fmonth_of_year("July" ++ Tail)     -> {7, Tail};
fmonth_of_year("August" ++ Tail)   -> {8, Tail};
fmonth_of_year("September" ++ Tail)-> {9, Tail};
fmonth_of_year("October" ++ Tail)  -> {10, Tail};
fmonth_of_year("November" ++ Tail) -> {11, Tail};
fmonth_of_year("December" ++ Tail) -> {12, Tail}.
