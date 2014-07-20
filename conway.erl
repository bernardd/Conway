-module(conway).

-compile(export_all).

% This module implements the actual Game of Life algorithm.
% It works in the following manner:
% * Collect a list of all cells that require checking. That's any cell that is alive or
%   adjacent to a live one (including diagonally)
% * Iterate through the list checking the new state of each cell
% * If that cell should be changed from its current state, add it to a list of changes
% * Once all cells have been processed, push that list of changes back to the cell store

% Run a single step of the game
run_step() ->
	% Figure out all the changed cells
	Changes = lists:foldl(fun maybe_change/2, [], cells_to_process()),
	% Apply the changes
	[cell_store:set_cell(C, State) || {C, State} <- Changes],
	ok.

% foldl funtion to figure out if a cell needs changing and add it to the list if it does
maybe_change(C, Acc) ->
	CurrState = cell_store:get_cell(C),
	% Only record the cell if it's next state is different from its current one
	case next_state(C, CurrState) of
		CurrState -> Acc;                 % No change
		NewState -> [{C, NewState} | Acc] % Changed state - add to the list to update when we're done
	end.

% Return a list of all cells that need processing - that's all occupied cells, plus
% all cells adjacent (including diagonally) to an occupied cell.
cells_to_process() ->
	lists:usort(lists:flatten([cells_to_process(C) || C <- cell_store:all_cells()])).

% Get all cells adjacent to a given cell as well as the cell itself
cells_to_process({XIn, YIn}) ->
	[{X, Y} || X <- adjacent_values(XIn), Y <- adjacent_values(YIn)].

% Calculate the next state for a cell ('true' for occupied, 'false' for empty)
next_state(Pos, CurrentState) ->
	case {adjacent_live_cells(Pos), CurrentState} of
		{2, true} -> true; % 2 neighbours on a live cell keeps it alive
		{3, _} -> true;    % 3 neighbours on any cell keeps/turns it alive
		_ -> false         % Anything else kills/leaves the cell
	end.

% Count the number of live cells adjacent to the given one
% (not including the cell itself)
adjacent_live_cells({XIn, YIn}) ->
	length([
			true ||
			X <- adjacent_values(XIn),
			Y <- adjacent_values(YIn),
			{X, Y} =/= {XIn, YIn},
			cell_store:get_cell({X, Y})
		]).

% Trivial function to return the supplied value
% plus the values one on either side
adjacent_values(I) -> [I-1, I, I+1].
