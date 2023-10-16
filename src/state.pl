:- consult('utils.pl').

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
game_state(player_white, Board, []). % TODO: como eu meto aqui a board?


% Switches the player that is going to make the next move
% switch_player(+CurrentPlayer, -NextPlayer)
switch_player(player_white, player_black).
switch_player(player_black, player_white).


% createBoard(+Board)
createBoard(Board):-
	write('Please enter a size of board between 0 and 26: '),
	read_namber(Size), clear_buffer,
	SizeOfEmpty is Size - 4,
	createListOfPieces(Size, white, WhitePieces),
	createListOfPieces(Size, black, BlackPieces),
	createListOfPieces(Size, empty, EmptyPieces),
	appendNTimes([], [empty|BlackPieces], 2, L1),
	appendNTimes(L1, [empty|EmptyPieces], SizeOfEmpty, L2),
	appendNTimes(L2, [empty|WhitePieces], 2, Board).

% Create the list with Size - 1 of Pieces, but the lust piece is 'empty'.
% createListOfPieces(+Size, -List, +Piece).
createListOfPieces(2, _, [empty]).
createListOfPieces(Size, Piece, [Piece|T]):-
	Size1 is Size - 1,
	createListOfPieces(Size1, Piece, T).

% appendNTimes(+OriginList, +ToAppend, +N, -Result).
appendNTimes(OriginList, _, 0, OriginList).
appendNTimes(OriginList, ToAppend, N, [ToAppend|T]):-
	N1 is N - 1,
	appendNTimes(OriginList, ToAppend, N1, T).