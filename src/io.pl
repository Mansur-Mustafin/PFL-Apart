:- ensure_loaded('main.pl').

read_pos(none-none, false).
read_pos(Col-Row, true) :-
	write('Enter a valid position in this format Column-Row: '), nl,
	read_collumn(Col),
	read_dash,
	read_row(Row),
	!.

read_pos(none-none, true) :-
	peek_char(Char),
	Char = '\n',
	get_char(_), !.

read_pos(Col-Row, true) :-
	clear_buffer,
	read_pos(Col-Row, true).

read_collumn(Col) :-
	peek_code(Char),
	validate_letter(Char, Letter),
	Col is Letter - 65, 
	get_code(_), !.

validate_letter(Letter, Letter) :-
	Letter >= 65,
	Letter =< 90.

validate_letter(Char, Letter) :-
	Char >= 97,
	Char =< 122,
	Letter is Char - 32.

read_dash :-
	peek_char(Char),
	Char = '-',
	get_char(_).

read_row(Row) :-
	read_row_aux(0, false, X),
	Row is X - 1.

read_row_aux(Acc, _, X):-
	peek_code(C),
	C >= 48,
	C =< 57,
	get_code(_),
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_row_aux(Acc1, true, X).

read_row_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer.

% read_number(X, Low, Up). 
read_number(X, Low, Up):-
	repeat,
	read_number_aux(0, false, X),
	between(Low, Up, X),
	!.

read_number_aux(Acc, _, X):-
	peek_code(C),
	C >= 48,							% '0'
	C =< 57,
	get_code(_),						% '9'
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_number_aux(Acc1, true, X).

read_number_aux(0, false, _):-
	write('Please enter number'), nl,
	clear_buffer, fail.


read_number_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer, !.

read_number_aux(X, true, X):-
	peek_char(C),
	C \= '\n',
	write('Please enter number'), nl,
	clear_buffer,
	fail.


% clear buffer/0
clear_buffer:-
	repeat,
	get_char(C),
	C = '\n',
	!.


get_mode(Level) :-
	print_main_menu,
	read_number(Level, 1, 4).

get_board_size(NumCol-NumRow) :-
	write('Please enter a size of board columns between 4 and 26: '), nl,
	read_number(NumCol, 4, 26),
	write('Please enter a size of board rows grater then 6: '), nl,
	read_number(NumRow, 6, 50).

