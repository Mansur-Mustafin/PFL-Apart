% display_piece(+Piece)
display_piece(empty):- write(' ').
display_piece(black):- write('b').
display_piece(white):- write('w').

% display_game(+Game_State)
display_game([CurrentPlayer, Board, _]) :-
	display_board(Board).

% display_board(+Board).
display_board([H|T]):-
	length(H, Len),
	display_header(1, Len),
	draw_board_map([H|T], 1, Len).

% description
% display_header(+Cur, +Len)
display_header(Cur, Len):-
	write('    '),
	display_sequence_letters(Cur, Len),
	write('  '), draw_between_line(1, Len).

% description
% display_sequence_letters(+Cur, +Len)
display_sequence_letters(Len, Len):-
	Char is Len + 64,
	put_code(Char), nl.

display_sequence_letters(Cur, Len):-
	Char is Cur + 64,
	put_code(Char), write('   '),
	Cur1 is Cur + 1,
	display_sequence_letters(Cur1, Len).

% description
% draw_between_line(+Cur, +Len)
draw_between_line(Len, Len):-
	write('|---|'), nl.

draw_between_line(Cur, Len):-
	write('|---'),
	Cur1 is Cur + 1,
	draw_between_line(Cur1, Len).

% description
% draw_number_line(+Board, +CurVal, +Len)
draw_board_map([H], Len, Len):-
	write(Len), write(' '),
	display_sequence_pieces(H),
	write('  '), draw_between_line(1, Len).

draw_board_map([H|T], CurVal, Len):-
	write(CurVal), write(' '),
	display_sequence_pieces(H),
	write('  '), draw_between_line(1, Len),
	CurVal1 is CurVal + 1,
	draw_board_map(T, CurVal1, Len).

% description
% display_sequence_pieces(+RowOfPieces)
display_sequence_pieces([]):-
	write('|'), nl.

display_sequence_pieces([Piece|T]):-
	write('| '),
	display_piece(Piece),
	write(' '),
	display_sequence_pieces(T).