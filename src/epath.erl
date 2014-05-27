%% @description
%%   XPath-like query for Erlang terms
%%
%% @deprecated
%%   use pairs:lookup/2 pairs:get/2
-module(epath).

-export([
   q/2
]).

q([Key | Tail], Term) ->
   q(Tail, q_term(Key, Term));
q([], Term) ->
   Term.


q_term(_, undefined) ->
   undefined;
q_term(Key, X)
 when is_tuple(X) ->
   erlang:element(Key, X);
q_term(Key, X)
 when is_integer(Key), is_list(X) ->
   lists:nth(Key, X);
q_term(Key, X)
 when is_list(X) ->
   case lists:keyfind(Key, 1, X) of
      false    -> undefined;
      {_, Val} -> Val
   end.
