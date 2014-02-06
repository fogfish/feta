%% @description
%%   file system utility
-module(fs).

-export([
   foreach/2
]).

-type(path() :: list()).

%%
%% applies a function to each file for its side-effects;
%% it returns nothing.
-spec(foreach/2 :: (function(), path()) -> ok).

foreach(Fun, Path) ->
   case filelib:is_dir(Path) of
      true  ->
         case file:list_dir(Path) of
            {ok, List} ->
               lists:foreach(
                  fun(X) ->
                     foreach(Fun, X)
                  end,
                  [filename:join(Path, X) || X <- List]
               );
            {error, _Reason} ->
               ok
         end;
      false -> 
         Fun(Path)
   end.
