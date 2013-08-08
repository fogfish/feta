-module(osproc).
-behaviour(gen_server).

-export([
   start/1, start/2, start/3,
   start_link/1, start_link/2, start_link/3,
   init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-define(BOOTSTRAP, "
   echo \"pkill -9 $$\";
   exec ~s ~s
").

%%
%%
start(Cmd) ->
   start(Cmd, []).
start(Cmd, Args) ->
   gen_server:start(?MODULE, [Cmd, Args], []).
start(Name, Cmd, Args) ->
   gen_server:start({local, Name}, ?MODULE, [Cmd, Args], []).

start_link(Cmd) ->
   start_link(Cmd, []).
start_link(Cmd, Args) ->
   gen_server:start_link(?MODULE, [Cmd, Args], []).
start_link(Name, Cmd, Args) ->
   gen_server:start_link({local, Name}, ?MODULE, [Cmd, Args], []).


init([Cmd, Args]) ->
   erlang:process_flag(trap_exit, true),
   Port = erlang:open_port(
      {spawn_executable, os:find_executable(sh)},
      [
         {args, ["-c", build_cmd(Cmd, Args)]},
         exit_status, use_stdio, stderr_to_stdout
      ]
   ),
   {ok, {Port, undefined}}.

build_cmd(Cmd, Args)
 when is_atom(Cmd) ->
   io_lib:format(?BOOTSTRAP, [
      os:find_executable(Cmd),
      string:join(Args, " ")
   ]);

build_cmd(Cmd, Args)
 when is_list(Cmd) orelse is_binary(Cmd) ->
   io_lib:format(?BOOTSTRAP, [
      Cmd,
      string:join(Args, " ")
   ]).


terminate(_, {Port, undefined}) ->
   maybe_close_port(erlang:port_info(Port), Port),
   ok;

terminate(_, {Port, Kill}) ->
   maybe_close_port(erlang:port_info(Port), Port),
   os:cmd(Kill),
   ok.

maybe_close_port(undefined, _) ->
   ok;
maybe_close_port(_, Port) ->
   erlang:port_close(Port).

%%
%%
handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info({_Port, {exit_status, 0}}, S) ->
   {stop, normal, S};

handle_info({_Port, {exit_status, Reason}}, S) ->
   {stop, {?MODULE, Reason}, S};

handle_info({_, {data, [$p, $k, $i, $l, $l | _]=Kill}}, {Port, _}) ->
   {noreply, {Port, Kill}};

handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _) ->
   {ok, S}.





