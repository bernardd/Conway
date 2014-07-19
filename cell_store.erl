-module(cell_store).

-compile(export_all). % Lazy

-define(TABLE, cell_store).

init() ->
	ets:new(?TABLE, [public, named_table, {read_concurrency, true}]).

set_cell(Pos, true) ->
	ets:insert(?TABLE, {Pos, true});

set_cell(Pos, false) ->
	ets:delete(?TABLE, Pos).

get_cell(Pos) ->
	ets:lookup(?TABLE, Pos) =/= [].

all_cells() ->
	ets:tab2list(?TABLE).

save() ->
	{ok, Dets} = dets:open_file(?TABLE, []),
	ets:to_dets(?TABLE, Dets), % to_dets clears the dets table first, so no need to do it explicitly
	dets:close(Dets).
	
load() ->
	{ok, Dets} = dets:open_file(?TABLE, []),
	ets:delete_all_objects(?TABLE), % from_dets doesn't empty the table - do it ourselves
	ets:from_dets(?TABLE, Dets),
	dets:close(Dets).
