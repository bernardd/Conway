-module(printer).

-compile(export_all).

print() ->
	print({0,0}, {100, 100}).

print({StartX, StartY}, {FinX, FinY}) ->
	[print_row(StartX, FinX, Y) || Y <- lists:seq(StartY, FinY)],
	io:fwrite("~n", []),
	ok.

print_row(StartX, FinX, Y) ->
	[io:fwrite("~s", [print_cell({X, Y})]) || X <- lists:seq(StartX, FinX)],
	io:fwrite("~n", []).

print_cell(Cell) ->
	case cell_store:get_cell(Cell) of
		true -> "X";
		false -> "."
	end.
