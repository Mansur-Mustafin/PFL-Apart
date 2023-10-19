:- use_module(library(lists)).

% TODO: issue with 2 Enters.
% read_number(X).
read_namber(X):-
	read_namber_aux(0, false, X).

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
	clear_buffer,
	read_namber(X).


read_namber_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer.

read_namber_aux(X, true, X):-
	peek_char(C),
	C \= '\n',
	write('Please enter number'), nl,
	clear_buffer,
	read_namber(X).


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

