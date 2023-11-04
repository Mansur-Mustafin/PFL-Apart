% display_piece(+Piece)
display_piece(empty):- write(' ').
display_piece(black):- write('b').
display_piece(white):- write('w').
display_piece(visited):- write('x').
display_piece(white_selected):- write('W').
display_piece(black_selected):- write('B').
display_piece(valid):- write('V').

selected_piece(black, black_selected).
selected_piece(white, white_selected).


% display_game(+Game_State)
display_game([CurrentPlayer, Board, Visited]) :-
	display_player(CurrentPlayer),
	show_valid_moves(CurrentPlayer, Board, Visited, NewBoard1),
	process_visited(NewBoard1, Visited, true, NewBoard2),
	display_board(NewBoard2).


% display_board(+Board).
display_board([H|T]):-
	shape([H|T], NumberRows, NumberCol),
	display_header(1, NumberCol),
	draw_board_map([H|T], 1, NumberRows, NumberCol).


% Dislay the header of board 
% display_header(+Cur, +Len)
display_header(Cur, Len):-
	write('     '),
	display_sequence_letters(Cur, Len),
	write('   '), draw_between_line(1, Len).


% Display Len number capitals letters.
% display_sequence_letters(+Cur, +Len)
display_sequence_letters(Len, Len):-
	Char is Len + 64,
	put_code(Char), nl.

display_sequence_letters(Cur, Len):-
	Char is Cur + 64,
	put_code(Char), write('   '),
	Cur1 is Cur + 1,
	display_sequence_letters(Cur1, Len).


% Draw line between pieces.Ex: |---|---...---|---|
% draw_between_line(+Cur, +Len)
draw_between_line(Len, Len):-
	write('|---|'), nl.

draw_between_line(Cur, Len):-
	Cur < Len,
	write('|---'),
	Cur1 is Cur + 1,
	draw_between_line(Cur1, Len).


% Display the Board with all pieces.
% draw_number_line(+Board, +CurVal, +NRow, +NCol).
draw_board_map([H], NRows, NRows, NCol):-
	write(NRows), write(' '),
	display_sequence_pieces(H),
	write('   '), draw_between_line(1, NCol).

draw_board_map([H|T], CurVal, NRow, NCol):-
	writeNumber(CurVal),
	display_sequence_pieces(H),
	write('   '), draw_between_line(1, NCol),
	CurVal1 is CurVal + 1,
	draw_board_map(T, CurVal1, NRow, NCol).


% Write the Number with margin.
% writeNumber(+Number)
writeNumber(X):-
	X < 10,
	write(X), write('  '), !.
writeNumber(X):-
	write(X), write(' ').


% description
% display_sequence_pieces(+RowOfPieces)
display_sequence_pieces([]):-
	write('|'), nl.

display_sequence_pieces([Piece|T]):-
	write('| '),
	display_piece(Piece),
	write(' '),
	display_sequence_pieces(T).


% Just print the titel (name of game).
% display_titel/0
display_titel:-
	write(' ========================='), nl,
	write(' |                       |'), nl,
	write(' |       A P A R T       |'), nl,
	write(' |                       |'), nl,
	write(' ========================='), nl.


% Write whose turn it is now. 
% display_player(+Player)
display_player(player_white):-	
	write('Turn of player playing white'), nl.
display_player(player_black):-	
	write('Turn of player playing black'), nl.
display_player(easy_pc_white):-	
	write('Turn of pc playing white'), nl.
display_player(easy_pc_black):-	
	write('Turn of pc playing black'), nl.


% true = The first element on Board = selected.
process_visited(Board, [], _, Board).

process_visited(Board, [Col-Row|T], true, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	selected_piece(Value, SelectedValue),
	set_value_at(Board, Row, Col, SelectedValue, TempBoard),
	process_visited(TempBoard, T, false, NewBoard).

process_visited(Board, [Col-Row|T], false, NewBoard):-
	process_visited(Board, T, false, TempBoard),
	set_value_at(TempBoard, Row, Col, visited, NewBoard).

show_valid_moves(_, Board, [], Board) :- !.
show_valid_moves(Player, Board, _, Board) :- \+ is_human(Player), !.
show_valid_moves(Player, Board, Visited, NewBoard) :-
	valid_moves_player(Player-Board-Visited, Player, ValidMoves),
	process_valid_moves(ValidMoves, Board, NewBoard).

process_valid_moves([], NewBoard, NewBoard).
process_valid_moves([ValidCol-ValidRow | T], Board, NewBoard) :-
	get_value_at(Board, ValidRow, ValidCol, empty),
	set_value_at(Board, ValidRow, ValidCol, valid, NextBoard),
	process_valid_moves(T, NextBoard, NewBoard).

process_valid_moves([ValidCol-ValidRow | T], Board, NewBoard) :-
	get_value_at(Board, ValidRow, ValidCol, Value),
	selected_piece(Value, SelectedValue),
	set_value_at(Board, ValidRow, ValidCol, SelectedValue, NextBoard),
	process_valid_moves(T, NextBoard, NewBoard).
