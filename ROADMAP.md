# Roadmap

The eventual goal of this project is to make it usable for searching code,
refactoring code, performing static analysis (as a declarative linter), as
well as supporting more languages.

For now the focus is on querying Erlang codebases.

## Current focus

  - [ ] Have a CLI application to interact with the Glass query service;
  - [ ] Support querying all forms in Erlang ASTs;
  - [ ] Support Erlang source files (requires work on parsing);
  - [ ] Support querying macros;
  - [ ] Support multi-entity queries carrying unification environments across them;
  - [ ] Support showing bound variables in the query instead of full matches;
  - [ ] Add more relational logic and other operators to the query language
    (e.g.: `unordered(<query>)` and `any(<query>)`);