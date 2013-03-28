%% @description
%%
-module(clock).

-export([
   set/2, reset/2, cancel/1
]).


%%
%% set alarm once after time
set(T, Msg)
 when is_integer(T) ->
   {clock, T, erlang:send_after(T, self(), Msg)};

set({clock, _T, _Timer}=X, _Msg) ->
   X.

%%
%% re-set alarm after time
reset(T, Msg)
 when is_integer(T) ->
   clock:set(T, Msg);

reset({clock, T, Timer}, Msg) ->
   erlang:cancel_timer(Timer),
   clock:set(T, Msg).

%%
%%
cancel(T)
 when is_integer(T) ->
   T;

cancel({clock, T, Timer}) ->
   erlang:cancel_timer(Timer),
   T.


