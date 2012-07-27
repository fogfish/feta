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
-module(format).

-export([datetime/2]).
-define(UNX_EPOCH, 62167219200).


%%%----------------------------------------------------------------------------   
%%%
%%% date/time formater 
%%%
%%%----------------------------------------------------------------------------   

%%
%%
datetime(Fmt, {{_Y,_M,_D}, {_H,_N,_S}}=Val) ->
   datetime(Fmt, Val, []);

datetime(Fmt, {_Mega, _Sec, _Micro}=Now) ->
   datetime(Fmt, calendar:now_to_universal_time(Now), []);

datetime(Fmt, Val) when is_integer(Val) ->
   datetime(Fmt, calendar:gregorian_seconds_to_datetime(Val + ?UNX_EPOCH), []).


%% Day 
%% %a  An abbreviated textual representation of the day Sun through Sat
%% %A  A full textual representation of the day  Sunday through Saturday
%% %d  Two-digit day of the month (with leading zeros) 01 to 31
%% %e  Day of the month, with a space preceding single digits. 1 to 31
%% %j  Day of the year, 3 digits with leading zeros  001 to 366
%% %u  ISO-8601 numeric representation of the day of the week  1 (for Monday) though 7 (for Sunday)
%% %w  Numeric representation of the day of the week 0 (for Sunday) through 6 (for Saturday)
datetime([$%, $a | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   datetime(Tail, Val, [day_of_week(Day) | Acc]);  

datetime([$%, $A | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   datetime(Tail, Val, [fday_of_week(Day) | Acc]);  

datetime([$%, $d | Tail], {{_Y,_M,D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [D]) | Acc]);

datetime([$%, $e | Tail], {{_Y,_M,D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10. B", [D]) | Acc]);

datetime([$%, $j | Tail], {{Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Y-1, 12, 31),
   datetime(Tail, Val, [io_lib:format("~3.10.0B", [Day]) | Acc]);

datetime([$%, $u | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = calendar:day_of_the_week(Date),
   datetime(Tail, Val, [io_lib:format("~B", [Day]) | Acc]);

datetime([$%, $w | Tail], {{_Y,_M,_D}=Date, {_H,_N,_S}}=Val, Acc) ->
   Day = case calendar:day_of_the_week(Date) of
      7 -> 0;
      D -> D
   end,
   datetime(Tail, Val, [io_lib:format("~B", [Day]) | Acc]);

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
datetime([$%, $b | Tail], {{_Y, M,_D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [month_of_year(M) | Acc]);

datetime([$%, $B | Tail], {{_Y, M,_D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [fmonth_of_year(M) | Acc]);

datetime([$%, $m | Tail], {{_Y,M,_D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [M]) | Acc]);

%%
%% Year
%% %C  Two digit representation of the century (year divided by 100, truncated to an integer)  19 for the 20th Century
%% %g  Two digit representation of the year going by ISO-8601:1988 standards (see %V)  Example: 09 for the week of January 6, 2009
%% %G  The full four-digit version of %g Example: 2008 for the week of January 3, 2009
%% %y  Two digit representation of the year  Example: 09 for 2009, 79 for 1979
%% %Y  Four digit representation for the year  Example: 2038

datetime([$%, $y | Tail], {{Y, _M,_D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [Y rem 1000])  | Acc]);

datetime([$%, $Y | Tail], {{Y, _M,_D}, {_H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~4.10.0B", [Y])  | Acc]);

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

datetime([$%, $H | Tail], {{_Y, _M,_D}, {H,_N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [H])  | Acc]);

datetime([$%, $M | Tail], {{_Y, _M,_D}, {_H,N,_S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [N])  | Acc]);

datetime([$%, $S | Tail], {{_Y, _M,_D}, {_H,_N,S}}=Val, Acc) ->
   datetime(Tail, Val, [io_lib:format("~2.10.0B", [S])  | Acc]);

%% Time and Date Stamps
%% %c  Preferred date and time stamp based on local  Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
%% %D  Same as "%m/%d/%y"  Example: 02/05/09 for February 5, 2009
%% %F  Same as "%Y-%m-%d" (commonly used in database datestamps) Example: 2009-02-05 for February 5, 2009
%% %s  Unix Epoch Time timestamp (same as the time() function) Example: 305815200 for September 10, 1979 08:40:00 AM
%% %x  Preferred date representation based on locale, without the time Example: 02/05/09 for February 5, 2009
%%
%% Miscellaneous
%% %n  A newline character ("\n")  ---
%% %t  A Tab character ("\t")
datetime([H | Tail], Val, Acc) ->
   datetime(Tail, Val, [H | Acc]);

datetime([], _Val, Acc) ->
   lists:flatten(lists:reverse(Acc)).


%%-------------------------------------------------------------------
%%
%% private
%%
%%-------------------------------------------------------------------

%%
%% day of week
day_of_week(1) -> "Mon";
day_of_week(2) -> "Tue";
day_of_week(3) -> "Wed";
day_of_week(4) -> "Thu";
day_of_week(5) -> "Fri";
day_of_week(6) -> "Sat";
day_of_week(7) -> "Sun".

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

