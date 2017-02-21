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
%%   List of configuration options - extension to proplists that transparently 
%%    * read options from key/value pairs
%%    * read options from application environment
%%    * read options from OS environment
%%
%%   For example, read opts from list of pairs
%%   opts:get(key, [...]).  
%%  
%%   For example, read opts from application
%%   opts:get(key, myapp).
%%
%%   The config file http://erlang.org/doc/man/config.html might define a value
%%     {key, {env, "OS_ENV_VAR", default}}
%%   it is run-time expanded to actual value of OS_ENV_VAR
-module(opts).

-export([check/3, check/2, filter/2, get/2, get/3, val/2, val/3]).
-export_type([opts/0]).

%%
%% data types
-type key()   :: _.

-type val()   :: _.
-type pair()  :: {key(), val()} | key().

-type app()   :: atom().
-type opts()  :: [pair()] | app().


-define(is_app(X),   is_atom(X)).
-define(is_opts(X),  is_list(X)).

%%
%% check-and-define option with given key
-spec check(key(), val(), opts()) -> opts().

check(Key, Default, Opts) ->
   case fail(Key, opts:get(Key, Default, Opts) ) of
      {_, Default} ->
         append(Key, Default, Opts);
      _ ->
         Opts
   end.

%%
%% check collection of keys are defined at environment
-spec check([pair()], opts()) -> opts().

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
 when ?is_opts(Opts) ->
   [Pair || Pair <- Opts, lists:member(key(Pair), Filter)];

filter(Filter, App)
 when ?is_app(App) ->
   filter(Filter, application:get_all_env(App)).


%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec get(key() | [key()], opts()) -> pair().

get(Key, Opts) ->
   fail(Key, opts:get(Key, undefined, Opts) ).

%%
%% read options value, return default value if key do not exists
-spec get(key(), val(), opts()) -> pair().

get(Key, Default, Opts)
 when ?is_opts(Opts) ->
   get_opts(Key, Default, Opts);

get(Key, Default, Opts)
 when ?is_app(Opts) ->
   get_app(Key, Default, Opts).


%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec val(key(), opts()) -> val().

val(Key, Opts) ->
   value( opts:get(Key, Opts) ).

%%
%% read option value, return default value if key do not exists
-spec val(key(), val(), opts()) -> val().

val(Key, Default, Opts) ->
   value( opts:get(Key, Default, Opts) ).

%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

%%
%% read value from options pair list
get_opts(Key, Default, Opts) ->
   case lists:keyfind(Key, 1, Opts) of
      {Key, _}=Val ->
         Val;
      _          -> 
         case lists:member(Key, Opts) of
            true  -> Key;
            false -> {Key, Default}
         end
   end.

%%
%%
get_app(Key, Default, App) ->
   case application:get_env(App, Key) of
      {ok, true} -> Key;
      {ok, {env, Env, Def}} -> get_os_env(Key, Env, Def, App);
      {ok, Val} -> {Key, Val};
      undefined  -> {Key, Default}
   end.

%%
%%
get_os_env(Key, Env, Def, App) ->
   Val = os:getenv(Env, Def),
   application:set_env(App, Key, Val),
   {Key, Val}.   


%%
%%
append(Key, Default, Opts)
 when ?is_opts(Opts) ->
   [{Key, Default} | Opts];

append(Key, Default, App)
 when ?is_app(App) ->
   application:set_env(App, Key, Default),
   App.


%%
%% fails if none : maps pair() to error()
fail(Key, {_, undefined}) ->
   throw({badarg, Key});
fail(_, Pair) ->
   Pair.

%%
%% return pair value
value({_, Value}) ->
   Value;
value(_) ->
   true.

%%
%% return pair key
key({Key, _}) ->
   Key;
key(Key) ->
   Key.
