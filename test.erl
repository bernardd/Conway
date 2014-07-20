-module(test).

-compile(export_all).

% Some handy functions for testing the Game of Life

% Set up an initial state with three different life forms
setup_example() ->
	cell_store:init(),
	life_forms:add(life_forms:blinker(), {54, 55}),
	life_forms:add(life_forms:block(), {60, 60}),
	life_forms:add(life_forms:glider(), {50,50}),
	printer:print().

% Set up an initial state with four gliders
setup_gliders() ->
	cell_store:init(),
	life_forms:add(life_forms:rotate_r(life_forms:rotate_r(life_forms:glider())), {30, 30}),
	life_forms:add(life_forms:rotate_r(life_forms:glider()), {20, 40}),
	life_forms:add(life_forms:rotate_l(life_forms:glider()), {40, 40}),
	life_forms:add(life_forms:glider(), {30, 50}),
	printer:print().

% Iterate the game with 400ms delay between each step.
% Prints the grid from 0,0 to 100,100 at each step.
run() ->
	printer:print(),
	Pid = spawn(fun loop/0),
	io:get_line(""), % Wait until Enter is pressed
	Pid ! stop.

loop() ->
	conway:run_step(),
	printer:print(),
	receive
		stop -> ok
	after 400 ->
		loop()
	end.
