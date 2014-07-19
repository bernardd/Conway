-module(conway).

-compile(export_all). % Lazy :)

run_step() ->
	Changes = lists:foldl(fun maybe_change/2, [], cells_to_process()),
	[cell_store:set_cell(C, State) || {C, State} <- Changes].

maybe_change(C, Acc) ->
	CurrState = cell_store:get_cell(C),
	case next_state(C, CurrState) of
		CurrState -> Acc; % No change
		NewState -> [{C, NewState} | Acc] % Changed state - add to the list to update when we're done
	end.

cells_to_process() ->
	lists:usort([cells_to_process(C) || C <- cell_store:all_cells()]).

cells_to_process({XIn, YIn}) ->
	[{X, Y} || X <- adjacent_values(XIn), Y <- adjacent_values(YIn)].

next_state(Pos, CurrentState) ->
	case {adjacent_live_cells(Pos), CurrentState} of
		{2, true} -> true; % 2 neighbours on a live cell keeps it alive
		{3, _} -> true; % 3 neighbours on any cell keeps/turns it alive
		_ -> false % Anything else kills/leaves the cell
	end.

adjacent_live_cells({XIn, YIn}) ->
	length([
			true ||
			X <- adjacent_values(XIn),
			Y <- adjacent_values(YIn),
			{X, Y} =/= {XIn, YIn},
			cell_store:get_cell({X, Y})
		]).

adjacent_values(I) -> [I-1, I, I+1].
