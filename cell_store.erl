-module(cell_store).

-compile(export_all).

% This module provides a simple interface to an ETS table for storing/retrieving the location of
% living cells.

-define(TABLE, cell_store).

% Set up an empty cell store
init() ->
	case ets:info(?TABLE) of
		undefined ->
			ets:new(?TABLE, [public, named_table]);
		_ ->
			ets:delete_all_objects(?TABLE)
	end.

% Set the state of a cell ('true' for occupied, 'false' otherwise)
set_cell(Pos, true) ->
	% Wrap the coordinates in a tuple so that the index is on the tuple {X,Y} rather
	% than just a single coordinate
	ets:insert(?TABLE, {Pos});
set_cell(Pos, false) ->
	ets:delete(?TABLE, Pos).

% Get the value of a cell
get_cell(Pos) ->
	ets:lookup(?TABLE, Pos) =/= [].

% Get a list of all occupied cells
all_cells() ->
	[C || {C} <- ets:tab2list(?TABLE)].

% Save the current state to a persistant store
save() ->
	{ok, Dets} = dets:open_file(?TABLE, []),
	ets:to_dets(?TABLE, Dets), % to_dets clears the dets table first, so no need to do it explicitly
	dets:close(Dets).

% Load a state saved with save/0
load() ->
	{ok, Dets} = dets:open_file(?TABLE, []),
	ets:delete_all_objects(?TABLE), % ets:from_dets doesn't empty the table - do it ourselves
	ets:from_dets(?TABLE, Dets),
	dets:close(Dets).
