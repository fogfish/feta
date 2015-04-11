%% @description
%%
-module(deb).

-export([
   uptime/0
  ,queue/0
  ,queue/1
  ,heap/0
  ,heap/1
  ,cpu/0
  ,cpu/1
  ,supervised/1
  ,state/1
]).

%%
%%
uptime() ->
   {UpTime, _}    = erlang:statistics(wall_clock),
   {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
   lists:flatten(io_lib:format("~p days, ~p:~p:~p", [D,H,M,S])).


%%
%% inspect length of process queues
-spec(queue/0 :: () -> [{pid(), integer()}]).
-spec(queue/1 :: ([pid()]) -> [{pid(), integer()}]).

queue() ->
   queue(erlang:processes()).

queue(Pids) ->
   pid_info(message_queue_len, Pids).

%%
%% inspect processes heap size
-spec(heap/0 :: () -> [{pid(), integer()}]).
-spec(heap/1 :: ([pid()]) -> [{pid(), integer()}]).

heap() ->
   heap(erlang:processes()).

heap(Pids) ->
   pid_info(total_heap_size, Pids).

%%
%% inspect reductions used by processes
-spec(cpu/0 :: () -> [{pid(), integer()}]).
-spec(cpu/1 :: ([pid()]) -> [{pid(), integer()}]).

cpu() ->
   cpu(erlang:processes()).

cpu(Pids) ->
   pid_info(reductions, Pids).

%%
%% return list of all worker processes (incl. nested) visible in tree
-spec(supervised/1 :: (atom() | pid()) -> [pid()]).

supervised(Sup) ->
   supervised(Sup, []).

supervised(Sup, Acc0) ->
   lists:foldl(
      fun
         ({_,Pid,worker,_},     Acc) -> [Pid|Acc];
         ({_,Pid,supervisor,_}, Acc) -> supervised(Pid, Acc)
      end,
      Acc0,
      supervisor:which_children(Sup)
   ).

%%
%% read process state
-spec(state/1 :: (atom() | pid()) -> any()).

state(Pid) ->
   {status, _Pid, _Mod, 
      [_PDict, _State, _Parent, _Dbg, Status]
   } = sys:get_status(Pid, 5000),
   case 
      [ State || {data, [{"State", State}]} <- Status]
   of
      []   ->
         Status;
      List ->
         hd(List)
   end.



%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
pid_info(Attr, Pids) ->
   sort(
      lists:filter(
         fun({_, Info}) -> Info =/= undefined end,
         [{X, get_pid_info(Attr, X)} || X <- Pids]
      )
   ).

%%
%%
get_pid_info(Attr, Pid) ->
   case erlang:process_info(Pid) of
      undefined ->
         undefined;
      List      ->
         case lists:keyfind(Attr, 1, List) of
            false      -> undefined;
            {_, Value} -> Value
         end
   end.

%%
%%
sort(List) ->
   lists:reverse(lists:keysort(2, List)).

