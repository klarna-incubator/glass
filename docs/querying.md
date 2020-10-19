# A Foundation for Code Queries

Glass aims to be a declarative tool for understanding and expressing rules
about programs. "Declarative" here means that one should be able to express
these concepts without performing any computation. A declarative foundation
gives the tool a lot more room for optimisation and tooling, as semantics
are not dependent on order of expressions.

Static analysis tools have a broad range of foundations. For Glass, we use
a form of unification with constraints over a program graph. The graph may
be enriched with control flow, data flow, and other information (such as
inferred types or effects). This is somewhat similar, but much less powerful,
to tools that translate programs to horn clauses and give you a complete
datalog engine to express queries in.


## Indexed forms

The first step in Glass is to index a codebase. Indexing will read some
serialisation of a program (source code, binary files, etc) and construct
a database of entities extracted from the source code. An entity may be
a module, a function, a class; anything of interest in a code base that
one wants to treat as a self-standing thing. This will vary between
languages, of course.

So a parser will not only turn source/binary code into some sort of tree, but
will also extract entities. These entities are then converted to Glass' internal
format---a standardised form of attributed graph, which may also contain
information about flow.

Once indexed, entities may be serialised to disk so this work doesn't need to be
performed again. And changes to the sources of these entities can trigger
incremental changes to the index, given a proper incremental implementation of
the parser. In this case, the ability of giving entities a *stable* identifier
at a fine-grained level, such that Glass can merge these entities and properly
infer changes between versions.


## The internal format

Glass' (conceptual) internal format is an attributed parse tree that *works* as
a graph. Each node in this tree has the following structure:

```erl
-record(glass_node, {
  id :: pos_integer(),
  tag :: atom(),
  properties :: #{ atom() => term() },
  attributes :: #{ term() => term() },
  children :: [#glass_node{}]
}).
```

Each node has:

  - an unique ID within its subgraph;
  - a tag that identifies the AST node (e.g.: `call` or `binary_operator`);
  - a set of intrinsic properties that are assigned by the parser (and which we
    expect to match against any other node with equivalent static semantics);
  - a set of attributes (or facts) that are derived from analysis or that
    provide additional insights about a node but which we do not want to use as
    an "intrinsic" semantic for it. In other words, properties that we don't
    automatically use for deciding node equality;
  - A list of child nodes.

These nodes are stored within entities. An entity has a stable identifier, along
with keeping an index of its children. This is what allows nodes to point
anywhere in the graph, rather than having them restricted to trees. A
`{stable_id(), node_id()}` tuple is guaranteed to properly and uniquely identify
a node in the graph even when code around it changes.

```erl
-record(glass_entity, {
  id :: term(),
  properties :: #{ atom() => term() },
  attributes :: #{ atom() => term() },
  children :: [#glass_node{}],
  children_index :: #{ pos_integer() => path() },
  dependencies :: [term()],
  dependants :: [term()]
}).
```

A workspace is, then, a bag of such entities along with the indexes defined for them.

```erl
-record(workspace, {
  id :: atom(),
  properties :: #{ atom() => term() },
  entities :: #{ term() => #glass_entity{} },
  indexes :: #{ atom() => index_trie() }
}).
```


## The language

Glass queries work on the internal Glass format. The language is influenced by
relational logic and constraint programming. Search works slightly differently
than what a logic programmer might expect, in parts due to Glass putting a
bigger focus on entities, rather than the result of unification itself.

These choices also mean that querying a codebase feels much less like
describing a system of logical relations, as one would do in Prolog or Datalog,
and much more as a programmer describing a pattern. "The code I'm looking for
looks like this, but I'm only interested in certain contexts/situations". For
example, should one be interested in calls to a logging function on a particular
file, a query could look like the following:

```erl
log(some_file, Message, Args)
```

In this case, `Message` and `Args` are freshly introduced logical variables,
and will unify with any subtree that may exist in those locations. Whereas
the other parts of this query are rigid program trees that will match equivalent
program trees elsewhere.

The use of unification allows programmers to express some common scenarios
concisely. For example, finding all cases of the identity function in an
Erlang codebase could be done through the query:

```erl
fun(X) -> X end
```

That is, a function that takes one argument, and simply returns this same
argument in its body.

And then, these patterns can be given constraints. A constraint is just a
logical expression that describes when the pattern should be valid. And this
expression can make use of any variable bound during unification. So, if one
would like to have all functions converting a number to a string, they could
write a query like this (assuming that something would provide type information):

```erl
fun(In) -> Out end
  when has_type(In, integer)
  and  has_type(Out, string)
```

Of course, the shape of these queries and the available predicates and relations
in a constraint depend on the language and the parser. In a Python codebase,
these could, instead, look like this:

```py
lambda input: output
if input :: integer and output :: string
```

### Formal definition

The query language can be defined as follows:

```
l in labels
x in variables
v in Erlang primitive values
n in node type names

Term t ::= n{l1 = v1, ..., lN = vN}(t1, ..., tN)        -- node pattern
         | t = t                                        -- equality relation
         | _                                            -- wildcard pattern
         | ___                                          -- slicing wildcard pattern
         | t1 or t2                                     -- pattern disjunction
         | t when c                                     -- constrained term

Constraint c ::= not c | c1 and c2 | c1 or c2 | c1 xor c2   -- logical ops
               | c =:= c | c =/= c | c > c | ...            -- relational ops
               | p(c1, ..., cN)                             -- primitive
               | x | v
```

A node pattern matches an exact node with properties `l = v...` and children `t...`.

An equality relation unifies two terms. With fresh variables this means that the
variable is bound to whatever the other side of the relation happens to be. With
bound variables and regular values, this means that the query fails if the
values are not the same.

Wildcard patterns (`'_'`) match any single node, whereas a slicing wildcard
pattern (`'___'`) matches any number of nodes.

A disjunction pattern matches the first pattern that matches. If the
disjunctions introduce distinct variables, this might mean that only some of the
variables get bound. (*TODO: this should probably be disallowed*).

A constrained term matches if the term matches **and** the constraint evaluates
to true. The constraint will have access to all variables bound up to that
point, evaluation proceeds left-to-right in applicative order in both pattern
unification and constraint evaluation.

> **TODO:**  
> Formal semantics for the query language.

It's up to each front-end parser to accept a language that is close enough to
the target programming language, and then compile that to the internal query
language Glass uses.
