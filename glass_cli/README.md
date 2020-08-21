# GlassCLI

## TODO List:
* create shell script for calling search query from terminal
* create shell script to start client/backend

## Test run with GlassBackend:
1. start glass_cli `iex --sname foo -S mix`
2. start glass_backend `rebar3 shell --name bar@localhost`
3. set cookie on glass_cli `:erlang.set_cookie(bar@localhost, {--backend_cookie--})`
4. index
```
iex > GlassCLI.index("glass", "/home/sophy/git/glass/glass_backend")
{:ok, :indexed}
```
5. run search
```
iex > GlassCLI.search("glass", "fun(Value) -> _ end")
```
6. after receiving :finish message
```
iex(foo@localhost)24> GlassCLI.search(query)
["%% in publish/3 at /home/sophy/git/glass/glass_backend/ebin/glass_query_server.beam:73\n74| fun (Result) -> publish_result(State, Form, Result) end\n\n",
 "%% in form_matches/2 at /home/sophy/git/glass/glass_backend/ebin/glass_query.beam:80\n85| fun ({A, B}) -> form_matches(A, B) end\n\n",
 "%% in unify/2 at /home/sophy/git/glass/glass_backend/ebin/glass_query.beam:21\n22| fun (Node) -> apply_query(Query, Node) end\n\n",
 "%% in get_all_files/1 at /home/sophy/git/glass/glass_backend/ebin/glass_index_server.beam:133\n135| fun (File) -> get_subdirectory_files(Root, File) end\n\n",
 "%% in beams_in_directory/1 at /home/sophy/git/glass/glass_backend/ebin/glass_index_server.beam:129\n131| fun (Filename) -> ends_with(\".beam\", Filename) end\n\n",
 "%% in index_beam/2 at /home/sophy/git/glass/glass_backend/ebin/glass_index_server.beam:82\n84| fun (Form) ->\n\tindex_beam_form(Workspace, {App, Module, Beam}, Form)\nend\n\n",
 "%% in index_workspace/2 at /home/sophy/git/glass/glass_backend/ebin/glass_index_server.beam:78\n80| fun (Beam) -> index_beam(Workspace, Beam) end\n\n",
 "%% in find_entities/2 at /home/sophy/git/glass/glass_backend/ebin/glass_index_server.beam:54\n67| fun ({Id, Form}) -> {Id, Form, find_metadata(Id)} end\n\n",
 "%% in search/2 at /home/sophy/git/glass/glass_backend/ebin/glass_ast.beam:398\n400| fun (N) -> search(N, Predicate) end\n\n",
 "%% in publish/3 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_query_server.beam:82\n83| fun (Result) -> publish_result(State, Form, Result) end\n\n",
 "%% in form_matches/2 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_query.beam:86\n92| fun ({A, B}) -> form_matches(A, B) end\n\n",
 "%% in unify/2 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_query.beam:27\n28| fun (Node) -> apply_query(Query, Node) end\n\n",
 "%% in get_all_files/1 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_index_server.beam:115\n117| fun (File) -> get_subdirectory_files(Root, File) end\n\n",
 "%% in beams_in_directory/1 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_index_server.beam:111\n113| fun (Filename) -> ends_with(\".beam\", Filename) end\n\n",
 "%% in index_beam/2 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_index_server.beam:73\n76| fun (Form) ->\n\tindex_beam_form(Workspace, {App, Module, Beam}, Form)\nend\n\n",
 "%% in index_workspace/3 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_index_server.beam:62\n65| fun (Beam) -> index_beam(Workspace, Beam) end\n\n",
 "%% in search/2 at /home/sophy/git/glass/glass_backend/_build/default/lib/glass_backend/ebin/glass_ast.beam:398\n400| fun (N) -> search(N, Predicate) end\n\n"]
```
