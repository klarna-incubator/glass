-module(glass).

-export([
  index/2,
  index/3,
  search/2,
  search/3
]).

index(Workspace, Path, Node) ->
  glass_index_server:index(Workspace, Path, Node).

index(Workspace, Path) ->
  glass_index_server:index(Workspace, Path, node()).

search(Workspace, Query, Node) ->
  glass_query_server:search(Workspace, Query, Node).

search(Workspace, Query) ->
  glass_query_server:search(Workspace, Query, node()).