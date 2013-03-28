%% @description
%%   XPath-like pattern matching for Erlang terms
-module(epath).

-export([
   q/2
]).

q([Key | Tail], Term) ->
   q(Tail, q(Key, Term));
q([], Term) ->
   Term;

q(Key, Term) 
 when is_list(Term), is_integer(Key), Key =/= 0, Key < length(Term) ->
   lists:nth(Key, Term);

q(Key, Term) 
 when is_list(Term) ->
   case lists:keyfind(Key, 1, Term) of
      false    -> undefined;
      {_, Val} -> Val
   end;

q(Key, Term)
 when is_tuple(Term), is_integer(Key), Key =/= 0, Key < size(Term) ->
   erlang:element(Key, Term);

q(_, _) ->
   undefined.
