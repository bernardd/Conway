-module(printer).

-compile(export_all).

% A very simple display system for the Game of Life cell store - just
% prints either "." for an empty cell or "X" for a full one.

% Print the default 100x100 grid, with 0,0 at the top left.
print() ->
	print({0,0}, {99, 99}).

% Print an arbitrary grid with StartX,StartY at the top left and FinX,FinY
% at the bottom right.
print({StartX, StartY}, {FinX, FinY}) ->
	[print_row(StartX, FinX, Y) || Y <- lists:seq(StartY, FinY)],
	io:fwrite("~n", []),
	ok.

%% Helper functions (not for external use):
print_row(StartX, FinX, Y) ->
	[io:fwrite("~s", [print_cell({X, Y})]) || X <- lists:seq(StartX, FinX)],
	io:fwrite("~n", []).

print_cell(Cell) ->
	case cell_store:get_cell(Cell) of
		true -> "X";
		false -> "."
	end.
