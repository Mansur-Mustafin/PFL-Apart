
read_pos(Col-Row) :-
	write('Enter a valid position in this format Column-Row: '), nl,
	read_collumn(Col),
	read_dash,
	read_row(Row),
	!.

read_pos(none-none) :-
	peek_char(Char),
	Char = '\n',
	get_char(_), !.

read_pos(Col-Row) :-
	clear_buffer,
	read_pos(Col-Row).

read_collumn(Col) :-
	peek_code(Char),
	validate_letter(Char, Letter),
	Col is Letter - 65, 
	get_code(_), !.

validate_letter(Letter, Letter) :-
	between(65, 90, Letter).

validate_letter(Char, Letter) :-
	between(97, 122, Char),
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
	C >= 48,							% '0'
	C =< 57,
	get_code(_),						% '9'
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_row_aux(Acc1, true, X).

read_row_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer.

% read_number(X, Low, Up). TODO: 
read_namber(X, Low, Up):-
	repeat,
	read_namber_aux(0, false, X),
	between(Low, Up, X),
	!.

read_namber_aux(Acc, _, X):-
	peek_code(C),
	C >= 48,							% '0'
	C =< 57,
	get_code(_),						% '9'
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_namber_aux(Acc1, true, X).

read_namber_aux(0, false, _):-
	write('Please enter number'), nl,
	clear_buffer, fail.


read_namber_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer.

read_namber_aux(X, true, X):-
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


% This function receive the letter and transform to number
% Ex: A -> 0, B -> 1, ...
% get_index(+Letter, -Code).
get_index(Letter, Code):-
	char_code(Letter, Code1),
	Code is Code1 - 65.


% basicly: Matrix[RowN][ColN] -> Value
% get_value_at(+Matrix, +RowN, +ColN, -Value)
get_value_at(Board, RowN, ColN, Value):-
    nth0(RowN, Board, Row),
    nth0(ColN, Row, Value).

% Set Value in Matrix at (RowN, ColN).
% set_value_at(+Matrix, +RowN, +ColN, +Value, -ResultMatrix)
set_value_at(Board, RowN, ColN, Value, NewBoard):-
    nth0(RowN, Board, Row),
    set_nth(Row, ColN, Value, NewRow),
    set_nth(Board, RowN, NewRow, NewBoard).


% Set Value in List at Index. Ex: List[Index] = Value
% set_nth(+List, +Index, +Value, -ResultList)
set_nth([_|T], 0, Value, [Value|T]).
set_nth([H|T], Index, Value, [H|Rest]):-
    Index > 0,
    NewIndex is Index - 1,
    set_nth(T, NewIndex, Value, Rest).


% Return the numbers of rows and columns in matrix.
% shape(+Matrix, -NumberOfRows, -NumberOfColumns).
shape([H|T], R, C):-
	length([H|T], R),
	length(H, C).


my_abs(X,X) :- X >= 0, !.
my_abs(X,Y) :- Y is -X.

my_map(_, [], _, []).
my_map(Pred, [H|T], Board, [NewValue|ResultList]):-
    G =.. [Pred, H, Board, NewValue], 
    G,
    my_map(Pred, T, Board, ResultList).
    
element_to_list(Col-Row, Board, [Col-Row]-NewBoard):-
    set_value_at(Board, Row, Col, empty, NewBoard).

add_end_turn(L, [none-none | L]).

get_board_index(Col-Row, RealCol-RealRow) :-
	Col1 is Col + 65,
	char_code(RealCol, Col1),
	RealRow is Row + 1.