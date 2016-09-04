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
%% @doc
%%   Configuration helper interface (extension to proplists). It transparently 
%%   resolves configuration options using either key/value pairs or application 
%%   global configuration: Erlang application environment and OS environment.
-module(opts).

-export([check/3, check/2, filter/2, get/2, get/3, val/2, val/3]).
-export_type([opts/0]).

-type key()   :: atom() | binary().
-type val()   :: term().
-type app()   :: atom().
-type pair()  :: {key(), val()}.
-type opts()  :: [pair() | key()] | app().


%%
%% check-and-define option with given key
-spec check(key(), val(), opts()) -> opts().

check(Key, Default, Opts)
 when not is_list(Key), is_list(Opts) ->
   case opts:get(Key, Default, Opts) of
      {_, undefined} ->
         throw({badarg, Key});
      {_, Default} ->
         [{Key, Default} | Opts];
      _ ->
         Opts
   end;

check(Key, Default, Opts)
 when not is_list(Key), is_atom(Opts) ->
   case opts:get(Key, Default, Opts) of
      {_, undefined} ->
         throw({badarg, Key});
      {_, Default} ->
         application:set_env(Opts, Key, Default),
         Opts;
      _ ->
         Opts
   end.


%%
%% check collection of keys are defined at environment
-spec check([pair() | key()], opts()) -> opts().

check([{Key, Default}|T], Opts) ->
   check(T, check(Key, Default, Opts));

check([Key|T], Opts) ->
   check(T, check(Key, undefined, Opts));
   
check([], Opts) ->
   Opts.


%%
%% perform white list filtering of supplied options
-spec filter([key()], opts()) -> opts().

filter(Filter, Opts)
 when is_list(Opts) ->
   lists:filter(
      fun
         ({X, _}) -> lists:member(X, Filter);
         (X)      -> lists:member(X, Filter)  
      end, 
      Opts
   );

filter(Filter, App)
 when is_atom(App) ->
   filter(Filter, application:get_all_env(App)).


%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec get(key() | [key()], opts()) -> pair().

get(Key, Opts)
 when not is_list(Key) ->
   case opts:get(Key, undefined, Opts) of
      {_, undefined} ->
         throw({badarg, Key});
      Value ->
         Value
   end;

get([Key|T], Opts) ->
   case opts:get(Key, undefined, Opts) of
      {_, undefined} -> get(T, Opts);
      Value          -> Value
   end;

get([], _Opts) ->
   throw(badarg).

%%
%% read option value, return default key if key do not exists
-spec get(key(), val(), opts()) -> pair().

get(Key, Default, Opts)
 when not is_list(Key), is_list(Opts) ->
   case lists:keyfind(Key, 1, Opts) of
      {Key, _}=Val ->
         Val;
      _          -> 
         case lists:member(Key, Opts) of
            true  -> Key;
            false -> {Key, Default}
         end
   end;

get(Key, Default, Opts)
 when not is_list(Key), is_atom(Opts) ->
   case application:get_env(Opts, Key) of
      {ok, true} -> Key;
      {ok,  Val} -> {Key, Val};
      undefined  -> {Key, Default}
   end;

get([Key|T], Default, Opts) ->
   case opts:get(Key, undefined, Opts) of
      {_, undefined} -> get(T, Default, Opts);
      Val            -> Val
   end;

get([], Default, _Opts) ->
   Default.

%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec val(key(), opts()) -> val().

val(Key, Opts) ->
   case opts:get(Key, Opts) of
      {_, Val} -> Val;
      _        -> true
   end.

%%
%% read option value, return default key if key do not exists
-spec val(key(), val(), opts()) -> val().

val(Key, Default, Opts) ->
   case opts:get(Key, Default, Opts) of
      {_, Val} -> Val;
      Result   -> Result
   end.

%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

