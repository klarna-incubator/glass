-module(glass_report).

-export([
  on_result/4
]).

on_result(_Workspace, _Id, Form, {_Env, Match}) ->
  show(Form, Match).

show({{function, {Name, Arity}}, Form, Meta}, Match) ->
  FormLine = glass_ast:get_line(Form),
  ErlangAst = case glass_ast:glass_to_node(Match) of
                Nodes when is_list(Nodes) -> Nodes;
                Node -> [Node]
              end,
  Line = glass_ast:get_line(Match),
  Code = erl_prettypr:format(erl_syntax:form_list(ErlangAst)),
  Filename = proplists:get_value(glass_filename, Meta),
  io:format("%% in ~p/~p at ~s:~p~n~p| ~s~n~n",
            [Name, Arity, Filename, FormLine, Line, Code]).