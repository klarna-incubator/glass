# Glass - a Generic Language Analysis and Search System

Glass aims to be a declarative tool for understanding and expressing rules about
programs. It sits somewhere between a semantic code search tool and a static
analysis tool---in the sense that you can build incremental static analysis on
top of Glass.

The design of Glass is guided by a particular developer experience vision. Here
we imagine a world where, rather than just reading code to understand their
systems, developers will be relying on tools to do the "heavy lifting". This
shifts the use of developer time to knowing which questions to ask, and how to
interpret summaries provided by tools. That is, instead of using the source code
as The Truth, developers will supplement their knowledge of systems with a broad
range of tools. This allows them to observe different aspects of the system.

In a sense, this is partly true already. Developers rely on code, version
control history, logs, traces, code-coverage, type and effect systems, tests,
property-based models, documentation, and so on, and so forth. All of these
tools provide a small slice of what the system does and what the system *should*
do. More often than not, these tools also have their very own set of interfaces
and interactions---tools don't really speak with each other, so developers have
to manually correlate summaries across all tools.

What if there was a tool that could aggregate all of these perspectives and give
users a way to derive new insights by correlating data between tools and
presenting this data in the context of the system's code? What if such tool fit
right into a developer's IDE workflow, such that the developer would not need to
context switch while working on a hypothesis or trying to solve a problem?

These are the kind of questions that drive the vision and design of Glass.


## Glass' design

  - [A Foundation for Code Queries](./querying)
