-ifndef(__GLASS_INDEX_HRL__).
-define(__GLASS_INDEX_HRL__, true).

-record(glass_index_entry, {
  path,
  id,
  form
}).

-record(glass_index_metadata, {
  id,
  field,
  value
}).

-define(glass_filename, glass_filename).
-define(glass_position, glass_position).

-endif.