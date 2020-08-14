-module(glass_ast).

-include("glass_ast.hrl").

-export([
  ast_to_glass/1,
  node_to_glass/1,
  glass_to_node/1,
  map/2,
  search/2,
  get_children/1,
  get_line/1,
  get_attr/2
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
      position => {Line, 0}
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


%%%_* Glass -> Erlang =================================================
glass_to_node(Nodes) when is_list(Nodes) ->
  lists:map(fun glass_to_node/1, Nodes);
glass_to_node(Node) ->
  case Node#glass_node.type of
    root ->
      get_children(Node);
    %% Forms
    function ->
      {function, get_line(Node), 
                 get_attr(name, Node), 
                 get_attr(arity, Node),
                 glass_to_node(get_children(Node))};
    clause ->
      [Patterns, Guards, Body] = get_children(Node),
      {clause, get_line(Node),
               glass_to_node(Patterns),
               glass_to_node(Guards),
               glass_to_node(Body)};
    %% Literals
    atom ->
      {atom, get_line(Node), get_attr(value, Node)};
    char ->
      {char, get_line(Node), get_attr(value, Node)};
    float ->
      {float, get_line(Node), get_attr(value, Node)};
    integer ->
      {integer, get_line(Node), get_attr(value, Node)};
    string ->
      {string, get_line(Node), get_attr(value, Node)};
    %% Patterns
    match ->
      [Left, Right] = get_children(Node),
      {match, get_line(Node), glass_to_node(Left), glass_to_node(Right)};
    cons ->
      [Head, Tail] = get_children(Node),
      {cons, get_line(Node), glass_to_node(Head), glass_to_node(Tail)};
    nil ->
      {nil, get_line(Node)};
    binary_op ->
      [Left, Right] = get_children(Node),
      {op, get_line(Node), get_attr(operation, Node), glass_to_node(Left), glass_to_node(Right)};
    unary_op ->
      [Arg] = get_children(Node),
      {op, get_line(Node), get_attr(operation, Node), glass_to_node(Arg)};
    record_index ->
      [Field] = get_children(Node),
      {record_index, get_line(Node), get_attr(record, Node), glass_to_node(Field)};
    record ->
      {record, get_line(Node), get_attr(record, Node), glass_to_node(get_children(Node))};
    record_field ->
      [Key, Value] = get_children(Node),
      {record_field, get_line(Node), glass_to_node(Key), glass_to_node(Value)};
    tuple ->
      {tuple, get_line(Node), glass_to_node(get_children(Node))};
    var ->
      {var, get_line(Node), get_attr(name, Node)};
    %% Expressions
    block ->
      {block, get_line(Node), glass_to_node(get_children(Node))};
    'case' ->
      [Expr, Cases] = get_children(Node),
      {'case', get_line(Node), glass_to_node(Expr), glass_to_node(Cases)};
    'catch' ->
      [Expr] = get_children(Node),
      {'catch', get_line(Node), glass_to_node(Expr)};
    fun_named ->
      [Name, Arity] = get_children(Node),
      {'fun', get_line(Node), {function, glass_to_node(Name), glass_to_node(Arity)}};
    fun_named_external ->
      [Module, Name, Arity] = get_children(Node),
      {'fun', get_line(Node), {function, glass_to_node(Module), glass_to_node(Name), glass_to_node(Arity)}};
    'fun' ->
      {'fun', get_line(Node), {clauses, glass_to_node(get_children(Node))}};
    named_fun ->
      {named_fun, get_line(Node), get_attr(name, Node), glass_to_node(get_children(Node))};
    call ->
      [Callee, Args] = get_children(Node),
      {call, get_line(Node), glass_to_node(Callee), glass_to_node(Args)};
    remote ->
      [Module, Name] = get_children(Node),
      {remote, get_line(Node), glass_to_node(Module), glass_to_node(Name)};
    node_list ->
      glass_to_node(get_children(Node));
    unknown_node ->
      {atom, 0, unknown_node}
  end.

get_line(Node) ->
  {Line, _} = get_attr(position, Node, {-1, -1}),
  Line.

get_attr(Attr, Node) ->
  maps:get(Attr, Node#glass_node.attributes).

get_attr(Attr, Node, Default) ->
  maps:get(Attr, Node#glass_node.attributes, Default).

get_children(Node) ->
  Node#glass_node.children.

%%%_* Tree transformations ============================================
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