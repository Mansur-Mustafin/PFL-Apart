:- ensure_loaded('main.pl').


/*
    File: state.pl
    Description: 
    This file contains all the predicates that deal with the creation of the board,
    initial state of the game and identification of pieces, levels of difficulty of the players.
*/


% Determines if value is 'none'
is_none(none).


% Determines if value is 'empty'
is_empty(empty).


/*
	is_human(+Player)
	Description: is_human/1 determines if Player is a human player
*/
is_human(player_white).
is_human(player_black).


/*
	set_robot(+Difficulty, +Color, -Computer)
	Description: set_robot/3 unifies Computer with the appropriate computer depending on the difficulty level
	Level chosen and the Color of the player (white or black)
*/
set_robot(1, white, easy_pc_white).
set_robot(1, black, easy_pc_black).
set_robot(2, white, hard_pc_white).
set_robot(2, black, hard_pc_black).


/*
	level(+Computer, -Difficulty)
	Description: level/2 unifies Difficulty with the difficulty level (1 or 2) depending on the computer
	chosen Computer
*/
level(easy_pc_white, 1).
level(easy_pc_black, 1).
level(hard_pc_white, 2).
level(hard_pc_black, 2).


/*
	my_piece(+Player, -Color)
	Description: my_piece/2 unifies Color with the color of the pieces of the player Player
*/
my_piece(player_black, black).
my_piece(player_white, white).
my_piece(easy_pc_black, black).
my_piece(easy_pc_white, white).
my_piece(hard_pc_white, white).
my_piece(hard_pc_black, black).


/*
	same_piece(+Piece1, +Piece2, +SamePiece)
	Description: same_piece/3 checks if Piece1 and Piece2 are equal to SamePiece
*/
same_piece(white, white, white).
same_piece(black, black, black).


/*
	switch_player(+CurrentPlayer, -NextPlayer)
	Description: switch_player/2 swaps the player that just played with the next player
*/
switch_player(P1-P2, P2-P1).


/*
	createPlayer(+Mode, -Players)
	Description: createPlayer/2 unifies Players with the two players that will take part in the game
	depending on the game mode chosen
*/
createPlayer(1, player_white-player_black).
createPlayer(2, player_white-computer).
createPlayer(3, computer-player_black).
createPlayer(4, computer-computer).

/*
	choose_computer(+Player, +Color, -FinalPlayer)
	Description: choose_computer/3 unifies the correct player to FinalPlayer depending on the type of the
	player (human or computer) and the color of the pieces of the player
*/
choose_computer(computer, Color, Player) :- 
	choose_robot_menu_header(Color),
	choose_robot_menu,
	read_number(Difficulty, 1, 2),
	set_robot(Difficulty, Color, Player), !.

% Doesn't do any computation if the Player is human
choose_computer(Player, _, Player).


/*
	createBoard(+NumCol-NumRow, -Board)
	Description: createBoard/2 unifies Board with a list of lists that defines the game's board with
	NumRow rows and NumCol columns. The board will be composed of two rows full of black pieces expect
	the first and last column, followed by NumRow - 4 empty rows and ending with two rows full of 
	white pieces expect the first and last column
*/
createBoard(NumCol-NumRow, Board):-
	SizeOfEmpty is NumRow - 4,
	createListOfPieces(NumCol, white, WhitePieces),
	createListOfPieces(NumCol, black, BlackPieces),
	createListOfPieces(NumCol, empty, EmptyPieces),
	appendNTimes([], [empty|WhitePieces], 2, L1),
	appendNTimes(L1, [empty|EmptyPieces], SizeOfEmpty, L2),
	appendNTimes(L2, [empty|BlackPieces], 2, Board).


/*
	createListOfPieces(+Size, +Piece, -List)
	Description: createListOfPieces/3 creates a list of size (Size - 1) with an empty piece as the last element
	and with pieces Piece in the rest of the elements of the list
*/
createListOfPieces(2, _, [empty]).
createListOfPieces(Size, Piece, [Piece|T]):-
	Size1 is Size - 1,
	createListOfPieces(Size1, Piece, T).


% append N times the ToAppend to OriginList.
% appendNTimes(+OriginList, +ToAppend, +N, -Result).
/*
	appendNTimes(+OriginList, +ToAppend, +N, -Result)
	Description: appendNTimes/4 appends list ToAppend to list OriginList, N times resulting in list Result
*/
appendNTimes(OriginList, _, 0, OriginList).
appendNTimes(OriginList, ToAppend, N, [ToAppend|T]):-
	N1 is N - 1,
	appendNTimes(OriginList, ToAppend, N1, T).
