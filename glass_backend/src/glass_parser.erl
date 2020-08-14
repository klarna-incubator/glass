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
  erl_parse:parse_exprs(Tokens).
