:- ensure_loaded('main.pl').


/*  
    get_index(+Letter, -Code)
    get_index/2 description: receives a character Letter and unifies Code with the column 
    that Letter represents on the board
*/
get_index(Letter, Column):-
	char_code(Letter, Column1),
	Column is Column1 - 65.

/*
    get_value_at(+Board, ?RowN, ?ColN, -Value)
    get_value_at/4 description: unifies Value with the piece that is in the ColN column and RowN row
    of the game's Board. Can also be used to every piece in the board recursively
*/
get_value_at(Board, RowN, ColN, Value):-
    nth0(RowN, Board, _Row),
    nth0(ColN, _Row, Value).


/*
    set_value_at(+Board, +RowN, +ColN, +Value, -NewBoard)
    set_value_at/5 description: updates the piece in row RowN and column ColN in Board resulting in NewBoard
*/
set_value_at(Board, RowN, ColN, Value, NewBoard):-
    nth0(RowN, Board, Row),
    set_nth(Row, ColN, Value, NewRow),
    set_nth(Board, RowN, NewRow, NewBoard).


/*
    set_nth(+List, +Index, +Value, -ResultList)
    set_nth/4 description: sets the value in index Index of List to Value resulting in ResultList
*/

% Base case removes the element at the head of the list and replaces it by the value Value
set_nth([_|T], 0, Value, [Value|T]).

set_nth([H|T], Index, Value, [H|Rest]):-
    Index > 0,
    NewIndex is Index - 1,
    set_nth(T, NewIndex, Value, Rest).


/*
    shape(+Board, -Rows, -Cols)
    shape/3 description: unifies Rows and Cols with the number of rows and columns of the game's Board, respectively 
*/
shape([H|T], Rows, Cols):-
	length([H|T], Rows),
	length(H, Cols).

/*
    add_end_turn(+List, -ResultList)
    add_end_turn/2 description: Adds the move 'none-none' to the head of List resulting in ResultList
*/
add_end_turn(L, [none-none | L]).


/*
    get_board_index(+ColIndex-RowIndex, -BoardCol-BoardRow)
    get_board_index/2 description: given the indexes of the column and row ColIndex-RowIndex,
    unifies BoardCol-BoardRow with the equivalent of ColIndex-RowIndex in the game's board.
    For example, 0-0 corresponds to A-1, 0-1 corresponds to A-2, 1-0 corresponds to B-1.
*/
get_board_index(ColIndex-RowIndex, BoardCol-BoardRow) :-
	Col is ColIndex + 65,
	char_code(BoardCol, Col),
	BoardRow is RowIndex + 1.


/* TODO: trocar a ordem de Result e Value
    best_turns(+List, -Result, +Value)
    best_turns/3 description: given List, a list of pairs in the form Value-Turn, unifies Result with a list of turns
    associated with the value Value. List must be sorted in descending order of value and Value is the highest value
    of all turns.
*/

% Returns an empty list if List is empty
best_turns([], [], _).

best_turns([Value-Turn|T], [Turn|T2], Value):- !,
    best_turns(T, T2, Value).

% Returns an empty list in Result when we find the first turn that doesn't have value Value
best_turns(_, [], _).

/*
    in_bounds(+Board, +CurrPosCol-CurrPosRow)
    in_bounds/2 description: checks if CurrPosCol-CurrPosRow is a valid position in the board
*/
in_bounds([H | T], CurrPosCol-CurrPosRow) :-
    length([H | T], Rows),
    length(H, Cols),
    CurrPosCol >= 0,
    CurrPosRow >= 0,
    CurrPosRow < Rows,
    CurrPosCol < Cols.
