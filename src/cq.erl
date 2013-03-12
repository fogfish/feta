%% @description
%%   continuous query
-module(cq).

-export([new/2, new/3]).

-type cqf()   :: fun(() -> list() | undefined).
-type epoch() :: integer() | any().

%% internal state
-record(cq, {      
   epoch,    % start of interval (usec)
   chronon,  % length of interval 
   deadline, % next query deadline
   chunk,    %
   cqf
}).

%%
%%
-spec(new/2 :: (integer(), cqf()) -> lazy:lazy()).
-spec(new/3 :: (epoch(), integer(), cqf()) -> lazy:lazy()).


new(Chronon, Fun) ->
   S = #cq{
      epoch    = usec(),
      chronon  = Chronon,
      deadline = usec(),
      chunk    = [],
      cqf      = Fun
   },
   lazy:tl(
      lazy:new(undefined, fun() -> handle(S) end)
   ).

new(Epoch, Chronon, Fun)
 when is_integer(Epoch) ->
   S = #cq{
      epoch    = Epoch,
      chronon  = Chronon,
      deadline = Epoch,
      chunk    = [],
      cqf      = Fun
   },
   lazy:tl(
      lazy:new(undefined, fun() -> handle(S) end)
   ).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
handle(#cq{chunk=[], deadline=Deadline}=S) ->
   case usec() of
      X when X > Deadline ->
         eval(S);
      X  ->
         timer:sleep((Deadline - X) div 1000),
         eval(S)
   end;

handle(#cq{chunk=[Head | Tail]}=S) ->
   lazy:new(Head, fun() -> handle(S#cq{chunk=Tail}) end).

%%
%%
eval(#cq{cqf=Fun, epoch=Epoch, chronon=Chronon, deadline=Deadline}=S) ->
   case Fun(Epoch, Deadline) of
      % 
      [Head | Tail] ->
         lazy:new(Head, fun() -> handle(S#cq{epoch=Deadline, chunk=Tail, deadline=Deadline + Chronon}) end);
      % no data
      [] ->
         lazy:new(undefined, fun() -> handle(S#cq{epoch=Deadline, deadline=Deadline + Chronon}) end);
      % end of stream
      undefined     ->
         lazy:new()
   end.


%%
%%
usec() ->
   {Mega, Sec, Micro} = erlang:now(),
   (Mega * 1000000 + Sec) * 1000000 + Micro.
