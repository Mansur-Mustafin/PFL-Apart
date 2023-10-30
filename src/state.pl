
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
game_state(FirstPlayer-SecondPlayer, _Board, []):-
	write('Choose mode:'), nl,
	write('Player vs Player..[1]'), nl,
	write('Player vs PC......[2]'), nl,
	write('PC vs Player......[3]'), nl,
	write('PC vs PC..........[4]'), nl,
	read_namber(Lvl, 1, 4),
	createPlayer(Lvl, FirstPlayer-SecondPlayer).

is_none(none).

is_human(player_white).
is_human(player_black).

my_piece(player_black, black).
my_piece(player_white, white).
my_piece(pc_white, white).
my_piece(pc_black, black).

same_piece(white, white, white).
same_piece(black, black, black).

% Switches the player that is going to make the next move
% switch_player(+CurrentPlayer, -NextPlayer)
switch_player(P1-P2, P2-P1).

createPlayer(1, player_white-player_black).
createPlayer(2, player_white-pc_black).
createPlayer(3, pc_white-player_black).
createPlayer(4, pc_white-pc_black).

% Ask user the size of board and create it with appropriate rules.
% createBoard(+Board)
createBoard(Board):-
	% TODO: check the between() 4 < N < 26.
	write('Please enter a size of board columns between 4 and 26: '), nl,
	read_namber(NumCol, 4, 26),
	write('Please enter a size of board rows grater then 6: '), nl,
	read_namber(NumRow, 6, 50),

	SizeOfEmpty is NumRow - 4,
	createListOfPieces(NumCol, white, WhitePieces),
	createListOfPieces(NumCol, black, BlackPieces),
	createListOfPieces(NumCol, empty, EmptyPieces),
	appendNTimes([], [empty|WhitePieces], 2, L1),
	appendNTimes(L1, [empty|EmptyPieces], SizeOfEmpty, L2),
	appendNTimes(L2, [empty|BlackPieces], 2, Board).


% Create the list with Size - 1 of Pieces, but the lust piece is 'empty'.
% createListOfPieces(+Size, -List, +Piece).
createListOfPieces(2, _, [empty]).
createListOfPieces(Size, Piece, [Piece|T]):-
	Size1 is Size - 1,
	createListOfPieces(Size1, Piece, T).


% append N times the ToAppend to OriginList.
% appendNTimes(+OriginList, +ToAppend, +N, -Result).
appendNTimes(OriginList, _, 0, OriginList).
appendNTimes(OriginList, ToAppend, N, [ToAppend|T]):-
	N1 is N - 1,
	appendNTimes(OriginList, ToAppend, N1, T).

