-module(applib).

-export([
   boot/2, is_application/1
]).

%%
%% application bootstrap (development helper)
%% -module(myapp).
%% -export([start/0]).
%%
%% start() -> applib:boot(?MODULE, "./rel/files/dev.config").
-spec(boot/2 :: (atom(), list()) -> ok).

boot(App, Config) ->
   setenv(Config),
   boot(App).

boot(kernel) -> ok;
boot(stdlib) -> ok;
boot(App) when is_atom(App) ->
   maybe_boot(code:where_is_file(atom_to_list(App) ++ ".app"), App).

maybe_boot(non_existing, App) ->
   throw({no_app, App});
maybe_boot(AppFile, App) ->
   case lists:keyfind(App, 1, application:loaded_applications()) of
      false ->
         {ok, [{application, _, List}]} = file:consult(AppFile), 
         Apps = proplists:get_value(applications, List, []),
         lists:foreach(
            fun(X) -> 
               ok = case boot(X) of
                  {error, {already_started, X}} -> ok;
                  Ret -> Ret
               end
            end,
            Apps
         ),
         application:start(App);
      _ ->
         {error, {already_started, App}}
   end.

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
   setenv({?MODULE, Opts}).

%%
%% check if application is loaded
is_application(App)
 when is_atom(App) ->
   case lists:keyfind(App, 1, application:which_applications()) of
      false -> false;
      _     -> true
   end.


