-module(life_forms).

-compile(export_all).

% Still life:

block() ->
	[
		[x,x],
		[x,x]
	].

beehive() ->
	[
		[o,x,x,o],
		[x,o,o,x],
		[o,x,x,o]
	].

loaf() ->
	[
		[o,x,x,o],
		[x,o,o,x],
		[o,x,o,x],
		[o,o,x,o]
	].

boat() ->
	[
		[x,x,o],
		[x,o,x],
		[o,x,o]
	].

% Oscillators
blinker() ->
	[ [x,x,x] ].

toad() ->
	[
		[o,x,x,x],
		[x,x,x,o]
	].

beacon() ->
	[
		[x,x,o,o],
		[x,x,o,o],
		[o,o,x,x],
		[o,o,x,x]
	].

% Insert Pulsar here if you feel like typing it all out :)

% Spaceships
glider() ->
	[
		[o,o,x],
		[x,o,x],
		[o,x,x]
	].


% Utility

% Add a life form (in the form of a matrix, as above)
% with the specified coordinates as the top-left
add(Matrix, {X, Y}) ->
	lists:foldl(fun add_row/2, {X, Y}, Matrix),
	ok.

add_row(Row, {X, Y}) ->
	lists:foldl(fun add_cell/2, {X, Y}, Row),
	{X, Y+1}.

% "Add" an empty cell (do nothing, just increment the column counter)
add_cell(o, {X, Y}) -> {X+1, Y};
% Add a full cell:
add_cell(x, P={X, Y}) ->
	cell_store:set_cell(P, true),
	{X+1, Y}.

% Transformations that can be applied to life forms
mirror_x(Matrix) -> [lists:reverse(L) || L <- Matrix].
mirror_y(Matrix) -> lists:reverse(Matrix).
rotate_l(Matrix) -> rotate_l(Matrix, []).
rotate_r(Matrix) -> lists:reverse([lists:reverse(Row) || Row <- rotate_l(Matrix, [])]).

rotate_l(Matrix, Acc) when hd(Matrix) =:= [] -> Acc;
rotate_l(Matrix, Acc) ->
	rotate_l([tl(Row) || Row <- Matrix], [[hd(Row) || Row <- Matrix] | Acc]).

