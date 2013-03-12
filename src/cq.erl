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
   chunk,    % interim result
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
handle(#cq{chunk=[], epoch=Epoch, chronon=Chronon}=S) ->
   Deadline = Epoch + Chronon,
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
eval(#cq{cqf=Fun, epoch=Epoch, chronon=Chronon}=S) ->
   case Fun(Epoch, Epoch + Chronon) of
      % 
      [Head | Tail] ->
         lazy:new(Head, fun() -> handle(S#cq{epoch=Epoch + Chronon, chunk=Tail}) end);
      % no data
      [] ->
         handle(S#cq{epoch=Epoch + Chronon});
      % end of stream
      undefined     ->
         lazy:new()
   end.


%%
%%
usec() ->
   {Mega, Sec, Micro} = erlang:now(),
   (Mega * 1000000 + Sec) * 1000000 + Micro.
