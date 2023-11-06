:- ensure_loaded('main.pl').

/*
    File: view.pl
    Description: 
    This file contains all the predicates necessary for displaying the game state,
    player moves, and other user interface elements for the game.
*/


/*
	display_piece(+Piece)
	Description: display_piece/1 Writes the corresponding symbol for a given type of piece to the output.
*/
display_piece(empty):- write(' ').
display_piece(black):- write('b').
display_piece(white):- write('w').
display_piece(visited):- write('x').
display_piece(white_selected):- write('W').
display_piece(black_selected):- write('B').
display_piece(valid):- write('o').
display_piece(valid_eat):- write('O').


/*
	selected_piece(+Piece, -SelectedPiece)
	Description: selected_piece/1 Maps a Piece to its 'selected' state representation.
*/
selected_piece(black, black_selected).
selected_piece(white, white_selected).


/*
	display_title/0
	Description: display_title/0 Prints the game title on the output.
*/
display_title:-
	write(' ========================='), nl,
	write(' |                       |'), nl,
	write(' |       A P A R T       |'), nl,
	write(' |                       |'), nl,
	write(' ========================='), nl.


/*
	display_player(+Player)
	Description: display_player/1 Prints the current player's turn based on the Player argument.
*/
display_player(player_white):-	
	write('Turn of player playing white'), nl.

display_player(player_black):-	
	write('Turn of player playing black'), nl.

% Does not print turn for computer players.
display_player(_).


/*
	process_visited(+Board, +Visited, +Flag, -ProcessedBoard)
	Description: process_visited/4 Updates Board to ProcessedBoard by marking the visited cells.
	Flag: indicates whether the current cell is the first in the Visited list (selected piece).
*/
% Base case: when visited empty
process_visited(Board, [], _, Board).

% Case of the first element in Visited: mark it with the selected piece's state.
process_visited(Board, [Col-Row|T], true, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	selected_piece(Value, SelectedValue),
	set_value_at(Board, Row, Col, SelectedValue, TempBoard),
	process_visited(TempBoard, T, false, NewBoard).

% Case of the remaining Visited cells: mark them as visited.
process_visited(Board, [Col-Row|T], false, NewBoard):-
	process_visited(Board, T, false, TempBoard),
	set_value_at(TempBoard, Row, Col, visited, NewBoard).


/*
	show_valid_moves(+Player, +Board, +Visited, -NewBoard)
	Description: show_valid_moves/4 Unifies the NewBoard with Board with showed the possible moves of Human player. 
*/
show_valid_moves(_, Board, [], Board) :- !.

show_valid_moves(Player, Board, _, Board) :- \+ is_human(Player), !.

% Case for a human Player with non-empty Visited: calculate and show valid moves on NewBoard.
show_valid_moves(Player, Board, Visited, NewBoard) :-
	get_valid_jumps(Player-Board-Visited, Player, ValidMoves),
	process_valid_moves(ValidMoves, Board, NewBoard).


/*
	process_valid_moves(+ValidMoves, +Board, -NewBoard)
	Description: process_valid_moves/3 Processes all valid moves and marks squares as 'valid' or 'valid_eat'.
*/
process_valid_moves([], NewBoard, NewBoard).

% If the current square is to an empty square, mark it as 'valid'.
process_valid_moves([ValidCol-ValidRow | T], Board, NewBoard) :-
	get_value_at(Board, ValidRow, ValidCol, empty),
	set_value_at(Board, ValidRow, ValidCol, valid, NextBoard),
	process_valid_moves(T, NextBoard, NewBoard).

% If not empty, mark it as 'valid_eat'.
process_valid_moves([ValidCol-ValidRow | T], Board, NewBoard) :-
	set_value_at(Board, ValidRow, ValidCol, valid_eat, NextBoard),
	process_valid_moves(T, NextBoard, NewBoard).


/*
	display_board(+Board)
	Description: display_board/1 Prints all content of Board with coordinates and grid.
*/
display_board([H|T]):-
	shape([H|T], NumberRows, NumberCol),
	display_header(1, NumberCol),
	draw_board_map([H|T], 1, NumberRows, NumberCol).


/*
	display_header(+CurColumn, +NumberOfColumns)
	Description: display_header/2 Prints the board header consisting of column labels (assumed to be capital letters), Cur starts with 1.
*/
display_header(Cur, Len):-
	write('     '),
	display_sequence_letters(Cur, Len),
	write('   '), draw_between_line(1, Len).


/*
	draw_number_line(+Board, +CurVal, +NRow, +NCol).
	Description: Displays the Board row by row, with each row's number and its pieces.
*/
% Case when processing the last row of the Board.
draw_board_map([H], NRows, NRows, NCol):-
	writeNumber(NRows),
	display_sequence_pieces(H),
	write('   '), draw_between_line(1, NCol).

% Recursive case for rows other than the last.
draw_board_map([H|T], CurVal, NRow, NCol):-
	writeNumber(CurVal),
	display_sequence_pieces(H),
	write('   '), draw_between_line(1, NCol),
	CurVal1 is CurVal + 1,
	draw_board_map(T, CurVal1, NRow, NCol).


/*
	display_sequence_letters(+Cur, +Len)
	Description: display_sequence_letters/2 Displays a sequence of capital letters starting from Cur up to Len, representing column headers.
*/
display_sequence_letters(Len, Len):-
	Char is Len + 64,
	put_code(Char), nl.

display_sequence_letters(Cur, Len):-
	Char is Cur + 64,
	put_code(Char), write('   '),
	Cur1 is Cur + 1,
	display_sequence_letters(Cur1, Len).


/*
	display_sequence_pieces(+RowOfPieces)
	Description: display_sequence_pieces/1 Displays the pieces in a given row, formatted within grid cells.
*/
display_sequence_pieces([]):-
	write('|'), nl.

display_sequence_pieces([Piece|T]):-
	write('| '),
	display_piece(Piece),
	write(' '),
	display_sequence_pieces(T).


/*
	draw_between_line(+Cur, +Len)
	Description: draw_between_line/2 Draws a horizontal line with sections between pieces for a board grid
*/
draw_between_line(Len, Len):-
	write('|---|'), nl.

draw_between_line(Cur, Len):-
	Cur < Len,
	write('|---'),
	Cur1 is Cur + 1,
	draw_between_line(Cur1, Len).


/*
	display_pc_move(+Player, +OriginPos, +DestinationPos).
	Description: display_pc_move/3 Displays the sequence of moves made by the computer.
*/
% Case when there is no move (stopped moving).
display_pc_move(Player, _-_, none-none) :-
	write('Player '),
	my_piece(Player, Piece),
	write(Piece),
	write(' stoped moving').

% Case when there is a move from OriginPos to DestinationPos.
display_pc_move(Player, CurrCol-CurrRow, NewCol-NewRow) :-
	get_board_index(CurrCol-CurrRow, PrintCurrCol-PrintCurrRow),
	get_board_index(NewCol-NewRow, PrintNewCol-PrintNewRow),
	write('Player '),
	my_piece(Player, Piece),
	write(Piece),
	write(' moved the piece on '), write(PrintCurrCol-PrintCurrRow),
	write(' to '), write(PrintNewCol-PrintNewRow), nl.



/*
	choose_robot_menu_header(+Piece)
	Description: choose_robot_menu_header/1 Displays the header for choosing 
				the difficulty level of the computer player based on the player color.
*/
choose_robot_menu_header(white) :- 
	write('Choose the difficulty of the white computer'), nl.

choose_robot_menu_header(black) :- 
	write('Choose the difficulty of the black computer'), nl.


/*
	choose_robot_menu/0
	Description: choose_robot_menu/0 Displays the menu for choosing the computer's difficulty level.
*/
choose_robot_menu :-
	write('Easy.....[1]'), nl,
	write('Hard.....[2]'), nl.

/*
	print_main_menu/0
	Description: print_main_menu/0 Displays the menu for choosing the mode of the game.
*/
print_main_menu :-
	write('Choose mode:'), nl,
	write('Player vs Player........[1]'), nl,
	write('Player vs Computer......[2]'), nl,
	write('Computer vs Player......[3]'), nl,
	write('Computer vs Computer....[4]'), nl.

/*
	print_winner(+Piece).
	Description: print_winner/1 Displays a message: which player has won the game based on the color of the pieces.
*/
print_winner(white):-
    write('[WHITE] The player with white pieces wins!'), nl.

print_winner(black):-
    write('[BLACK] The player with black pieces wins!'), nl.

/*
	writeNumber(+Number).
	Description: writeNumber/1 Prints the given Number with appropriate margin spacing. 
*/
writeNumber(X):-
	X < 10,
	write(X), write('  '), !.

writeNumber(X):-
	write(X), write(' ').