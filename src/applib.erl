-module(applib).

-export([
   boot/2
  ,deps/1
  ,is_running/1
  ,is_loaded/1
  ,phase/1
]).

%%
%% application bootstrap (development helper)
%% -module(myapp).
%% -export([start/0]).
%%
%% start() -> applib:boot(?MODULE, "./rel/files/dev.config").
-spec(boot/2 :: (atom(), list()) -> ok).

boot(App, Config) ->
   %% the boot is composed of three phases
   %%  1. load all dependencies (setup default app environment)
   %%  2. Overlay default environment
   %%  3. start all applications
   ok = ensure_loaded(App),
   ok = setenv(Config),
   ensure_started(App).

%%
%% list application dependencies
-spec(deps/1 :: (atom()) -> [atom()]).

deps(App)
 when is_atom(App) ->
   deps(App, []). 

deps(kernel, Acc) ->
   Acc;
deps(stdlib, Acc) ->
   Acc;
deps(App, Acc) ->
   maybe_deps(code:where_is_file(atom_to_list(App) ++ ".app"), App, Acc).

maybe_deps(non_existing, App, _Acc0) ->
   exit({no_app, App});
maybe_deps(AppFile, App, Acc0) ->
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   Apps = proplists:get_value(applications, List, []),
   Acc1 = lists:foldl(
      fun(X, Acc) ->
         case lists:member(X, Acc) of
            true  -> Acc;
            false -> deps(X, Acc)
         end
      end,
      Acc0,
      Apps
   ),
   Acc1 ++ [App].

%%
%% check application status
-spec(phase/1 :: (atom()) -> running | loaded | undefined).

phase(App)
 when is_atom(App) ->
   maybe_running(is_running(App), App).

maybe_running(true,  _) ->
   running;
maybe_running(false, App) ->
   maybe_loaded(is_loaded(App), App).

maybe_loaded(true,  _) ->
   loaded;
maybe_loaded(false, _App) ->
   undefined.

%%
%% check if application is running
-spec(is_running/1 :: (atom()) -> true | false).

is_running(App)
 when is_atom(App) ->
   case lists:keyfind(App, 1, application:which_applications()) of
      false -> false;
      _     -> true
   end.

%%
%% check if application is loaded
-spec(is_loaded/1 :: (atom()) -> true | false).

is_loaded(App)
 when is_atom(App) ->
   case lists:keyfind(App, 1, application:loaded_applications()) of
      false -> false;
      _     -> true
   end.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% configure application from file
setenv({App, Opts})
 when is_list(Opts) ->
   lists:foreach(
      fun({K, V}) -> application:set_env(App, K, V) end,
      Opts
   );
setenv([X|_]=File)
 when is_number(X) ->
   {ok, [Cfg]} = file:consult(File),
   lists:foreach(fun setenv/1, Cfg);
setenv(Opts)
 when is_list(Opts) ->
   lists:foreach(fun setenv/1, Opts).


%%
%% load all application dependencies
ensure_loaded(App) ->
   lists:foldl(fun ensure_loaded/2, ok, deps(App)).

ensure_loaded(App, ok) ->
   case application:load(App) of
      {error, {already_loaded, _}} -> 
         ok;
      Any ->
         Any
   end;
ensure_loaded(_,Error) ->
   Error.

%%
%% boot all application dependencies
ensure_started(App) ->
   lists:foldl(fun ensure_started/2, ok, deps(App)).

ensure_started(App, ok) ->
   case application:start(App) of
      {error, {already_started, _}} -> 
         ok;
      Any ->
         Any
   end;
ensure_started(_,Error) ->
   Error.
