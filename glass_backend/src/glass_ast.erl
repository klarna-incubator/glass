-module(glass_ast).

-include("glass_ast.hrl").

-export([
  ast_to_glass/1,
  node_to_glass/1,
  map/2,
  search/2
]).

ast_to_glass(Ast) ->
  #glass_node{
    type = root,
    attributes = #{},
    children = lists:map(fun node_to_glass/1, Ast)
  }.

%% Forms
node_to_glass({function, Line, Name, Arity, Clauses}) ->
  #glass_node{
    type = function,
    attributes = #{
      position => {Line, 0},
      name => Name,
      arity => Arity
    },
    children = lists:map(fun node_to_glass/1, Clauses)
  };

node_to_glass({clause, Line, Patterns, Guards, Body}) ->
  #glass_node{
    type = clause,
    attributes = #{
      position => Line
    },
    children = [node_to_glass(Patterns), node_to_glass(Guards), node_to_glass(Body)]
  };

%% Literals
node_to_glass({atom, Line, Value}) ->
  #glass_node{
    type = atom,
    attributes = #{
      position => {Line, 0},
      value => Value
    },
    children = []
  };

node_to_glass({char, Line, Value}) ->
  #glass_node{
    type = char,
    attributes = #{
      position => {Line, 0},
      value => Value
    },
    children = []
  };

node_to_glass({float, Line, Value}) ->
  #glass_node{
    type = float,
    attributes = #{
      position => {Line, 0},
      value => Value
    },
    children = []
  };

node_to_glass({integer, Line, Value}) ->
  #glass_node{
    type = integer,
    attributes = #{
      position => {Line, 0},
      value => Value
    },
    children = []
  };

node_to_glass({string, Line, Value}) ->
  #glass_node{
    type = string,
    attributes = #{
      position => {Line, 0},
      value => Value
    },
    children = []
  };

%% Patterns
node_to_glass({bin, Line, Elements}) ->
  #glass_node{
    type = bin,
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Elements)
  };

node_to_glass({bin_element, Line, Pattern, Size, Tls}) ->
  #glass_node{
    type = bin_element,
    attributes = #{
      position => {Line, 0},
      pattern => Pattern,
      size => Size,
      tls => Tls
    },
    children = []
  };

node_to_glass({match, Line, Left, Right}) ->
  #glass_node{
    type = match,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Left), node_to_glass(Right)]
  };

node_to_glass({cons, Line, Head, Tail}) ->
  #glass_node{
    type = cons,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Head), node_to_glass(Tail)]
  };

node_to_glass({nil, Line}) ->
  #glass_node{
    type = nil,
    attributes = #{
      position => {Line, 0}
    },
    children = []
  };

node_to_glass({op, Line, Op, Left, Right}) ->
  #glass_node{
    type = binary_op,
    attributes = #{
      position => {Line, 0},
      operation => Op
    },
    children = [node_to_glass(Left), node_to_glass(Right)]
  };

node_to_glass({op, Line, Op, Arg}) ->
  #glass_node{
    type = unary_op,
    attributes = #{
      position => {Line, 0},
      operation => Op
    },
    children = [node_to_glass(Arg)]
  };

node_to_glass({record_index, Line, Name, Field}) ->
  #glass_node{
    type = record_index,
    attributes = #{
      position => {Line, 0},
      record => Name
    },
    children = [node_to_glass(Field)]
  };

node_to_glass({record, Line, Name, Fields}) ->
  #glass_node{
    type = record,
    attributes = #{
      position => {Line, 0},
      record => Name
    },
    children = lists:map(fun node_to_glass/1, Fields)
  };

node_to_glass({record_field, Line, Key, Value}) ->
  #glass_node{
    type = record_field,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Key), node_to_glass(Value)]
  };

node_to_glass({tuple, Line, Items}) ->
  #glass_node{
    type = tuple,
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Items)
  };

node_to_glass({var, Line, Name}) ->
  #glass_node{
    type = var,
    attributes = #{
      position => {Line, 0},
      name => Name
    },
    children = []
  };

%% Expressions
node_to_glass({block, Line, Body}) ->
  #glass_node{
    type = block,
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Body)
  };

node_to_glass({'case', Line, Expr, Cases}) ->
  #glass_node{
    type = 'case',
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Expr), node_to_glass(Cases)]
  };

node_to_glass({'catch', Line, Expr}) ->
  #glass_node{
    type = 'catch',
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Expr)]
  };

node_to_glass({'fun', Line, {function, Name, Arity}}) ->
  #glass_node{
    type = fun_named,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Name), node_to_glass(Arity)]
  };

node_to_glass({'fun', Line, {function, Module, Name, Arity}}) ->
  #glass_node{
    type = fun_named_external,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Module), node_to_glass(Name), node_to_glass(Arity)]
  };

node_to_glass({'fun', Line, {clauses, Clauses}}) ->
  #glass_node{
    type = 'fun',
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Clauses)
  };

node_to_glass({named_fun, Line, Name, Clauses}) ->
  #glass_node{
    type = named_fun,
    attributes = #{
      position => {Line, 0},
      name => Name
    },
    children = lists:map(fun node_to_glass/1, Clauses)
  };

node_to_glass({call, Line, Callee, Args}) ->
  #glass_node{
    type = call,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Callee), node_to_glass(Args)]
  };

node_to_glass({remote, Line, Module, Name}) ->
  #glass_node{
    type = remote,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Module), node_to_glass(Name)]
  };

node_to_glass(Nodes) when is_list(Nodes) ->
  #glass_node{
    type = node_list,
    attributes = #{},
    children = lists:map(fun node_to_glass/1, Nodes)
  };

node_to_glass(Node) ->
  #glass_node{
    type = unknown_node,
    attributes = #{ node => Node },
    children = []
  }.

map(Fun, Tree0) ->
  Tree1 = Fun(Tree0),
  Children = Tree1#glass_node.children,
  Tree1#glass_node{
    children = lists:map(fun(Child) -> map(Fun, Child) end, Children)
  }.

search(Node, Predicate) ->
  Children = Node#glass_node.children,
  OtherResults = lists:flatmap(fun(N) -> search(N, Predicate) end, Children),
  case Predicate(Node) of
    {true, Result} ->
      [Result | OtherResults];
    false -> OtherResults
  end.