-module(glass_query).
-include("glass_ast.hrl").

-export([
  parse/1,
  unify/2
]).

-type query() ::
  {bind, Variable :: atom()}
| {form, Type :: atom(), Attributes :: map(), Children :: [query()]}
| hole.


%%%_* API =============================================================
parse(Source) ->
  {ok, Tokens} = glass_parser:tokenise(Source ++ "."),
  case glass_parser:parse_exprs(Tokens) of
    {error, Error} -> {error, Error};
    {ok, [Expr]} ->
      glass_to_query(glass_ast:node_to_glass(Expr));
    {ok, Form} ->
      %% TODO parse form
      glass_to_query(glass_ast:node_to_glass(Form))
  end.

unify(Query, Form) ->
  glass_ast:search(Form, fun(Node) -> apply_query(Query, Node) end).

%%%_* Internal ========================================================
glass_to_query(#glass_node{type = var, attributes = #{ name := '_'}}) ->
  hole;
glass_to_query(#glass_node{type = var, attributes = #{ name := Name }}) ->
  {bind, Name};
glass_to_query(#glass_node{type = Type, attributes = Attrs, children = Children }) ->
  {form, Type, maps:without([position], Attrs), lists:map(fun glass_to_query/1, Children)}.

%%%_* Unification =====================================================
apply_query(Query, Node) ->
  case match_form(#{}, Query, Node) of
    false -> false;
    Env -> {true, {Env, Node}}
  end.

match_form(false, _, _) ->
  false;
match_form(Env, hole, _Form) ->
  Env;
match_form(Env, {bind, Variable}, Form) ->
  assign(Env, Variable, Form);
match_form(
  Env,
  {form, Type, Attrs, Children},
  #glass_node{type = Type1, attributes = Attrs1, children = Children1}
) ->
  case Type =:= Type1 andalso attributes_match(Attrs, Attrs1) of
    true ->
      match_forms(Env, Children, Children1);
    false ->
      false
  end.

match_forms(Env, [], []) ->
  Env;
match_forms(Env, [L | LRest], [R | RRest]) ->
  case match_form(Env, L, R) of
    false -> false;
    NewEnv -> match_forms(NewEnv, LRest, RRest)
  end;
match_forms(_, _, _) ->
  false.

assign(false, _, _) ->
  false;
assign(Env, Variable, Form) ->
  case maps:find(Variable, Env) of
    error -> Env#{ Variable => Form };
    {ok, BoundForm} ->
      case form_matches(Form, BoundForm) of
        true -> Env;
        false -> false
      end;
    _ -> false
  end.

form_matches(
  #glass_node{type = Type, attributes = Attrs1, children = Children1},
  #glass_node{type = Type, attributes = Attrs2, children = Children2}
) ->
  attributes_match(Attrs1, Attrs2)
    andalso length(Children1) =:= length(Children2)
    andalso lists:all(fun({A, B}) -> form_matches(A, B) end, lists:zip(Children1, Children2));
form_matches(_, _) -> false.

attributes_match(Map1, Map2) ->
  minimal_attributes(Map1) =:= minimal_attributes(Map2).

minimal_attributes(Map) ->
  maps:without([position], Map).