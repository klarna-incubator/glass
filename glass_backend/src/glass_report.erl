-module(glass_report).

-export([
  on_result/4,
  pretty/4
]).

on_result(_Workspace, _Id, Form, {_Env, Match}) ->
  io:format(pretty(Form, Match)).

pretty(_Workspace, _Id, Form, {_Env, Match}) ->
  pretty(Form, Match).

pretty({{_, _, _, {function, {Name, Arity}}}, Form, Meta}, Match) ->
  FormLine = glass_ast:get_line(Form),
  ErlangAst = case glass_ast:glass_to_node(Match) of
                Nodes when is_list(Nodes) -> Nodes;
                Node -> [Node]
              end,
  Line = glass_ast:get_line(Match),
  Code = erl_prettypr:format(erl_syntax:form_list(ErlangAst)),
  Filename = proplists:get_value(glass_filename, Meta),
  io_lib:format("%% in ~p/~p at ~s:~p~n~p| ~s~n~n",
              [Name, Arity, Filename, FormLine, Line, Code]).