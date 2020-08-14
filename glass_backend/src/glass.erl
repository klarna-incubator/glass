-module(glass).

-export([
  index/2,
  search/2
]).

index(Workspace, Path) ->
  glass_index_server:index(Workspace, Path).

search(Workspace, Query) ->
  glass_query_server:search(Workspace, Query, node()).