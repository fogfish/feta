%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%% @description
%%   user agent parser - takes browser UA string and tokenize it to {product, vsn} tuples
%%   (see http://www.texsoft.it/index.php?c=software&m=sw.php.useragent&l=it)
-module(ua).

-export([decode/1]).

-type(prod() :: binary()).
-type(vsn()  :: binary() | undefined).

%%
%%             product[/version]  
%%                          netscape lang code             comments 
%%              1       2                              3   4       
-define(REGEX, "([^\s]*)([\s]*\[[a-zA-Z][a-zA-Z]\])?\s*(\\((([^()]|(\\([^()]*\\)))*)\\))?\s*").

%%
%% decode user agent string 
-spec decode(binary() | list()) -> [{prod(), vsn()}].

decode(UA)
 when is_binary(UA) ->
   % split user agent to list of tokens
   Tokens   = lists:map(
      fun decode_ua/1,
      re:split(UA, ?REGEX, [group])
   ),
   % filter invalid tokens and decode products
   [decode_product(X) || X <- lists:flatten(Tokens), X =/= <<>>];
decode(UA) ->
   decode(scalar:s(UA)).

%%
%%
decode_ua([_, Product, _, _, <<>>|_]) ->
   [Product];
decode_ua([_, Product, _, _, Comment|_]) ->
   Tokens = binary:split(Comment, [<<$,>>, <<$;>>, <<$ >>], [global, trim]),
   [[Product, Tokens]];
decode_ua(_) ->
   [].

%%
%%
decode_product(Token) ->
   case binary:split(Token, [<<$/>>]) of
      [Product]      -> 
         {to_lower(Product), undefined};
      [Product, Vsn] -> 
         {to_lower(Product), to_lower(Vsn)}
   end.

%%
%%
to_lower(X) ->
   << <<(unicode_to_lower(C))/utf8>> || <<C/utf8>> <= X >>.
unicode_to_lower(X)
 when X >= $A, X =< $Z ->
   $a + (X - $A);
unicode_to_lower(X) ->
   X.

