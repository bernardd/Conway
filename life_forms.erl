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
mirror_x(Matrix) -> [lists:reverse(L) || L <- Matrix].
mirror_y(Matrix) -> lists:reverse(Matrix).
rotate_l(Matrix) -> rotate_l(Matrix, []).
rotate_r(Matrix) -> lists:reverse([lists:reverse(Row) || Row <- rotate_l(Matrix, [])]).

rotate_l(Matrix, Acc) when hd(Matrix) =:= [] -> Acc;
rotate_l(Matrix, Acc) ->
	rotate_l([tl(Row) || Row <- Matrix], [[hd(Row) || Row <- Matrix] | Acc]).

