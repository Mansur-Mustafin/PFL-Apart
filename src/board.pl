:- consult('view.pl').


/* 	Player is the player currently making the move. 
	Board keeps which piece is in each cell of the board. The possible values are:
		empty: the cell is empty;
		white: there is a white piece in that cell;
		black: there is a black piece in that cell;
		curr_white: white piece that is currently being moved;
		curr_black: black piece that is currently being moved;
		visited: cells that have been visited in the current move (in a continuous jump).
*/
% game_state(+Player, +Board, +VisitedList)
game_state(player_white,
	[ 
	[empty, black, black, black, black, black, black, empty],
	[empty, black, black, black, black, black, black, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, white, white, white, white, white, white, empty],
	[empty, white, white, white, white, white, white, empty]
	],
	[]).


% Switches the player that is going to make the next move
% switch_player(+CurrentPlayer, -NextPlayer)
switch_player(player_white, player_black).
switch_player(player_black, player_white).



play :-
	game_state(Player, Board, Visited),
	display_game([Player, Board, Visited]).
