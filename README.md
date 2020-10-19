# Glass
> A semantic search tool for Erlang that supports large code-bases.

[![Build Status][ci-image]][ci-url]
[![License][license-image]][license-url]
[![Developed at Klarna][klarna-image]][klarna-url]


Glass is a tool for semantically searching source code, currently focusing on
Erlang. You can think of it as "grep, if grep could understand Erlang code".
For more information, read the [technical design documents](./docs).

## Usage example

```erlang
> glass:index(glass, "/path/to/glass").
> glass:search(glass, "maps:without([position], _)").

%% in glass_to_query/1 at ./_build/default/lib/glass_backend/ebin/glass_query.beam:25
30| maps:without([position], Attrs)

%% in minimal_attributes/1 at ./_build/default/lib/glass_backend/ebin/glass_query.beam:90
91| maps:without([position], Map)
```

_For more examples and usage, please refer to the [Docs](./docs)._

## Querying language

Glass uses a querying language based on pattern unification (and, in the future,
possibly with more logical operators). In practice, this means that in order
to query a codebase, you provide a piece of Erlang code that has variables where
expressions can go.

The query:

```erlang
maps:without([position], Arg)
```

Will match any `maps:without` call whose first argument is the exact list
`[position]`, and the second argument is any Erlang expression, which will be
bound to `Arg` within this query.

Variables are unified, which means that, just like in regular Erlang pattern
matching, you can use this to query pieces of code that have the same expression
in different places. A query for the identity function is:

```erlang
fun(A) -> A end
```

A query for taking the first element of a tuple would be:

```erlang
fun({A, _}) -> A end
```

This provides a simple foundation for querying code that is at the same time
very powerful. Particularly when extended for refactoring. A query like (syntax
not final):

```erlang
s/log(File, Format, [error])/log(error_log, Format ++ " file: ~p", [error, File])/
```

Could transform a piece of code like:

```erlang
log(debug, "Failed, reason=~p", [error])
```

Into:

```erlang
log(error_log, "Failed, reason=~p" ++ "file: ~p", [error, debug])
```

## Development setup

Glass requires both [OTP21+](https://www.erlang.org/downloads) and
[rebar3](https://www.rebar3.org/). `rebar3 shell` from the root will drop
you into an Erlang shell for Glass.

## Roadmap

The current focus is on making the query language feature-complete. And we
have plans to work on optimisation, refactoring, and more advanced (incremental)
static analysis in the future.

See our [roadmap](ROADMAP.md) document for more details.

## How to contribute

See our guide on [contributing](.github/CONTRIBUTING.md).

## Release History

See our [changelog](CHANGELOG.md).

## Etymology

Glass (/glas/) means "ice cream" in Swedish. English has a homonym with close
pronunciation that means "lens", in particular for the purpose of improving its
user's eyesight. This sounded like a fun name for a project meant to improve a
developer's understanding of their codebase by letting them "magnify" certain
aspects and patterns that can happen in big codebases.

After deciding on the name, the acronym "Generic Language and Search System" was
backfitted.

## License

Copyright © 2020 Klarna Bank AB

For license details, see the [LICENSE](LICENSE) file in the root of this project.


<!-- Markdown link & img dfn's -->
[ci-image]: https://img.shields.io/badge/build-passing-brightgreen?style=flat-square
[ci-url]: https://github.com/klarna-incubator/TODO
[license-image]: https://img.shields.io/badge/license-Apache%202-blue?style=flat-square
[license-url]: http://www.apache.org/licenses/LICENSE-2.0
[klarna-image]: https://img.shields.io/badge/%20-Developed%20at%20Klarna-black?labelColor=ffb3c7&style=flat-square&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAOCAYAAAAmL5yKAAAAAXNSR0IArs4c6QAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAALQAAAAAQAAAtAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABCgAwAEAAAAAQAAAA4AAAAA0LMKiwAAAAlwSFlzAABuugAAbroB1t6xFwAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAVBJREFUKBVtkz0vREEUhsdXgo5qJXohkUgQ0fgFNFpR2V5ClP6CQu9PiB6lEL1I7B9A4/treZ47c252s97k2ffMmZkz5869m1JKL/AFbzAHaiRbmsIf4BdaMAZqMFsOXNxXkroKbxCPV5l8yHOJLVipn9/vEreLa7FguSN3S2ynA/ATeQuI8tTY6OOY34DQaQnq9mPCDtxoBwuRxPfAvPMWnARlB12KAi6eLTPruOOP4gcl33O6+Sjgc83DJkRH+h2MgorLzaPy68W48BG2S+xYnmAa1L+nOxEduMH3fgjGFvZeVkANZau68B6CrgJxWosFFpF7iG+h5wKZqwt42qIJtARu/ix+gqsosEq8D35o6R3c7OL4lAnTDljEe9B3Qa2BYzmHemDCt6Diwo6JY7E+A82OnN9HuoBruAQvUQ1nSxP4GVzBDRyBfygf6RW2/gD3NmEv+K/DZgAAAABJRU5ErkJggg==
[klarna-url]: https://github.com/klarna-incubator
