:- ensure_loaded('main.pl').


% This function receive the letter and transform to number
% Ex: A -> 0, B -> 1, ...
% get_index(+Letter, -Code).
get_index(Letter, Code):-
	char_code(Letter, Code1),
	Code is Code1 - 65.

% basicly: Matrix[RowN][ColN] -> Value
% get_value_at(+Matrix, +RowN, +ColN, -Value)
get_value_at(Board, RowN, ColN, Value):-
    nth0(RowN, Board, _Row),
    nth0(ColN, _Row, Value).

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

add_end_turn(L, [none-none | L]).

get_board_index(Col-Row, RealCol-RealRow) :-
	Col1 is Col + 65,
	char_code(RealCol, Col1),
	RealRow is Row + 1.

% best(+List, -Result, +Value)
best_turns([], [], _).

best_turns([Value-Turn|T], [Turn|T2], Value):- !,
    best_turns(T, T2, Value).

best_turns(_, [], _).

in_bounds([H | T], CurrPosCol-CurrPosRow) :-
    length([H | T], Rows),
    length(H, Cols),
    CurrPosCol >= 0,
    CurrPosRow >= 0,
    CurrPosRow < Rows,
    CurrPosCol < Cols.
