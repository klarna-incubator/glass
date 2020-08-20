-module(glass_parser).

-export([
  tokenise/1,
  parse_exprs/1
]).

tokenise(Source) ->
  case erl_scan:string(Source) of
    {ok, Tokens, _End} -> {ok, Tokens};
    {error, Info, Loc} -> {error, {tokenise_failed, Info, Loc}}
  end.

parse_exprs(Tokens) ->
  % erl_parse supports different functions for parsing expression
  % or forms.
  % For differences between form and expression see:
  % https://erlang.org/doc/apps/erts/absform.html
  case erl_parse:parse_exprs(Tokens) of
    {error, _} -> erl_parse:parse_form(Tokens);
    Res -> Res
  end.
