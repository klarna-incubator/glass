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
node_to_glass({bin, Line, Elements}) ->
  #glass_node{
    type = bin,
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Elements)
  };

node_to_glass({bin_element, Line, Pattern, Size, TypeSpecs}) ->
  #glass_node{
    type = bin_element,
    attributes = #{
      position => {Line, 0},
      size => Size,
      type_specifiers => TypeSpecs
    },
    children = [node_to_glass(Pattern)]
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
node_to_glass({bc, Line, Expr, Qualifiers}) ->
  #glass_node{
    type = bc,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Expr), node_to_glass(Qualifiers)]
  };

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

node_to_glass({'if', Line, Cases}) ->
  #glass_node{
    type = 'if',
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Cases)
  };

node_to_glass({lc, Line, Expr, Qualifiers}) ->
  #glass_node{
    type = lc,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Expr), node_to_glass(Qualifiers)]
  };

node_to_glass({map, Line, Pairs}) ->
  #glass_node{
    type = map,
    attributes = #{
      position => {Line, 0}
    },
    children = lists:map(fun node_to_glass/1, Pairs)
  };

node_to_glass({map, Line, Origin, Pairs}) ->
  #glass_node{
    type = map_update,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Origin), node_to_glass(Pairs)]
  };

node_to_glass({'receive', Line, Cases}) ->
  #glass_node{
    type = 'receive',
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Cases)]
  };

node_to_glass({'receive', Line, Cases, After, Body}) ->
  #glass_node{
    type = 'receive_timeout',
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(After), node_to_glass(Body), node_to_glass(Cases)]
  };

node_to_glass({'try', Line, Body, Clauses, Catches, After}) ->
  #glass_node{
    type = 'try',
    attributes = #{
      position => {Line, 0}
    },
    children = [
      node_to_glass(Body),
      node_to_glass(Clauses),
      node_to_glass(Catches),
      node_to_glass(After)
    ]
  };

%% Qualifiers
node_to_glass({generate, Line, Pattern, Expr}) ->
  #glass_node{
    type = generate,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Pattern), node_to_glass(Expr)]
  };

node_to_glass({b_generate, Line, Pattern, Expr}) ->
  #glass_node{
    type = b_generate,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Pattern), node_to_glass(Expr)]
  };

%% Associations
node_to_glass({map_field_assoc, Line, Key, Value}) ->
  #glass_node{
    type = map_field_assoc,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Key), node_to_glass(Value)]
  };

node_to_glass({map_field_exact, Line, Key, Value}) ->
  #glass_node{
    type = map_field_exact,
    attributes = #{
      position => {Line, 0}
    },
    children = [node_to_glass(Key), node_to_glass(Value)]
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
    bin ->
      {bin, get_line(Node), glass_to_node(get_children(Node))};
    bin_element ->
      [Pattern] = get_children(Node),
      {bin_element, get_line(Node),
                    glass_to_node(Pattern),
                    get_attr(size, Node),
                    get_attr(type_specifiers, Node)};
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
    bc ->
      [Expr, Qualifiers] = get_children(Node),
      {bc, get_line(Node), glass_to_node(Expr), glass_to_node(Qualifiers)};
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
    'if' ->
      {'if', get_line(Node), glass_to_node(get_children(Node))};
    lc ->
      [Expr, Qualifiers] = get_children(Node),
      {lc, get_line(Node), glass_to_node(Expr), glass_to_node(Qualifiers)};
    map ->
      {map, get_line(Node), glass_to_node(get_children(Node))};
    'receive' ->
      {'receive', get_line(Node), glass_to_node(get_children(Node))};
    receive_timeout ->
      [After, Body, Cases] = get_children(Node),
      {'receive', get_line(Node),
                  glass_to_node(Cases),
                  glass_to_node(After),
                  glass_to_node(Body)};
    'try' ->
      [Body, Clauses, Catches, After] = get_children(Node),
      {'try', get_line(Node),
              glass_to_node(Body),
              glass_to_node(Clauses),
              glass_to_node(Catches),
              glass_to_node(After)};
    %% Qualifiers
    generate ->
      [Pattern, Expr] = get_children(Node),
      {generate, get_line(Node), glass_to_node(Pattern), glass_to_node(Expr)};
    b_generate ->
      [Pattern, Expr] = get_children(Node),
      {b_generate, get_line(Node), glass_to_node(Pattern), glass_to_node(Expr)};
    %% Associations
    map_field_assoc ->
      [Key, Value] = get_children(Node),
      {map_field_assoc, get_line(Node), glass_to_node(Key), glass_to_node(Value)};
    map_field_exact ->
      [Key, Value] = get_children(Node),
      {map_field_exact, get_line(Node), glass_to_node(Key), glass_to_node(Value)};
    node_list ->
      glass_to_node(get_children(Node));
    unknown_node ->
      {atom, 0, unknown_node};
    glass_special_node ->
      get_attr(ast, Node)
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