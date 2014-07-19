-module(test).

-compile(export_all).

run() ->
	cell_store:init(),
	[cell_store:set_cell(C, true) || C <- [{4, 5}, {5, 5}, {6, 5}]],
	[cell_store:set_cell(C, true) || C <- [{10, 10}, {11, 10}, {12, 10}, {10, 11}, {11, 11}, {12, 11}, {10, 12}, {11, 12}, {12, 12}]],
	printer:print({0,0}, {20,20}).
