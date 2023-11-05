:- ensure_loaded('main.pl').


get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :- 
    findall(NewCol-NewRow, (
        valid_move(Player-Board-[CurrCol-CurrRow | T], CurrCol-CurrRow-NewCol-NewRow)
        ), ValidMoves).


% valid_move(+GameState, ?Position)
valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow) :-
    get_value_at(Board, NewPosRow, NewPosCol, Dest),
    \+ my_piece(Player, Dest),

    \+ member(NewPosCol-NewPosRow, [CurrPosCol-CurrPosRow|T]),

    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, Direction, Distance),
    get_line_length(Board, CurrPosCol-CurrPosRow, Direction, Player, Distance),
    check_valid_jump([CurrPosCol-CurrPosRow|T], Distance).

% get_direction(+CurrPosition, NewPosition, -Direction, -Distance).
get_direction(SameCol-CurrRow, SameCol-NewRow, vertical, Distance) :- Distance is abs(CurrRow - NewRow).
get_direction(CurrCol-SameRow, NewCol-SameRow, horizontal, Distance) :- Distance is abs(CurrCol - NewCol).
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) > 0,
    Distance is abs(CurrCol - NewCol).
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) < 0,
    Distance is abs(CurrCol - NewCol).


% get_line_length(+Board, +CurrPosCol-CurrPosRow, +Direction, +Player, -Number).
get_line_length(Board, CurrPosCol-CurrPosRow, Direction, Player, Number) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol1-NewCurrPosRow1, Direction, left),
    explore_end(left, Board, NewCurrPosCol1-NewCurrPosRow1, Direction, Player, N1),
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol2-NewCurrPosRow2, Direction, right),
    explore_end(right, Board, NewCurrPosCol2-NewCurrPosRow2, Direction, Player, N2),
    Number is N1 + N2 + 1.


explore(SameCol-CurrRow, SameCol-NewRow, vertical, left) :- NewRow is CurrRow - 1.
explore(SameCol-CurrRow, SameCol-NewRow, vertical, right) :- NewRow is CurrRow + 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, left) :- NewCol is CurrCol - 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, right) :- NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, left) :- NewRow is CurrRow - 1, NewCol is CurrCol - 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, right) :- NewRow is CurrRow + 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, left) :- NewRow is CurrRow - 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, right) :- NewRow is CurrRow + 1, NewCol is CurrCol - 1.


explore_end(_, Board, CurrPosCol-CurrPosRow, _, _, 0) :- \+ in_bounds(Board, CurrPosCol-CurrPosRow), !.
explore_end(_, Board, CurrPosCol-CurrPosRow, _, Player, 0) :- 
    get_value_at(Board, CurrPosRow, CurrPosCol, Piece),
    \+ my_piece(Player, Piece), !.

explore_end(End, Board, CurrPosCol-CurrPosRow, Direction, Player, N) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol-NewCurrPosRow, Direction, End),
    explore_end(End, Board, NewCurrPosCol-NewCurrPosRow, Direction, Player, N1),
    N is N1 + 1, !.


check_valid_jump([_ | []], _) :- !.

check_valid_jump([SecondCol-SecondRow, FirstCol-FirstRow], Distance1) :-
    Distance1 > 1, !,
    get_direction(SecondCol-SecondRow, FirstCol-FirstRow, _, Distance),
    Distance > 1.

check_valid_jump([_, _ | _], Distance) :-
    Distance > 1.


% Se Primeiro turno e move de distancia 1
check_one_move_turn(Player-NextPlayer-_-[], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-_-[]) :-
    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, _, 1),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer).


% Qualquer move sem ser o de cima
check_one_move_turn(Player-NextPlayer-_-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, Player-NextPlayer-_-[NewPosCol-NewPosRow, CurrPosCol-CurrPosRow|T]).


find_all_pieces(Player, Board, ValidPieces) :-
    findall(Col-Row, check_valid_piece(Player, Board, Col-Row), ValidPieces).


valid_moves_aux(_-_, [], Answer, Answer) :- !. 
valid_moves_aux(Player-Board, [Col-Row | T], Acc, Answer) :-
    set_value_at(Board, Row, Col, empty, NewBoard),
    valid_moves_piece(Player-NewBoard, [[Col-Row]], [], ValidTurns),
    append(ValidTurns, Acc, Acc1),
    valid_moves_aux(Player-Board, T, Acc1, Answer).


valid_moves_piece(_-_, [], ValidTurns, ValidTurns):- !.
valid_moves_piece(Player-Board, CurrMoves, Acc, ValidTurns) :-
    get_next_moves(Player-Board, CurrMoves, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    valid_moves_piece(Player-Board, NewCurrMoves, Acc1, ValidTurns).


get_next_moves(_-_, [], NewMoves, NewMoves).
get_next_moves(Player-Board, [CurrMove | T], Acc, NewMoves) :-
    get_valid_jumps(Player-Board-CurrMove, Player, NextJumps),
    get_next_jumps(CurrMove, NextJumps, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    get_next_moves(Player-Board, T, Acc1, NewMoves).

get_next_jumps(_, [], NewMoves, NewMoves).
get_next_jumps(CurrMove, [CurrJump | T], Acc, NewMoves) :-
    get_next_jumps(CurrMove, T, [[CurrJump | CurrMove] | Acc], NewMoves).


check_valid_piece(Player, Board, Col-Row) :-
    get_value_at(Board, Row, Col, Value),
    my_piece(Player, Value),
    get_valid_jumps(Player-Board-[Col-Row], Player, [_ | _]).


valid_piece_choice(Player-NextPlayer, Board, Col-Row):-
    \+ is_none(Col),
    check_valid_piece(Player, Board, Col-Row),
    game_loop(Player-NextPlayer, Board, [Col-Row]).

valid_piece_choice(Player-NextPlayer, Board, _):-
    write('Please choose again.'), nl,
    game_loop(Player-NextPlayer, Board, []).


has_move(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T], true) :-
    valid_moves(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], Player, [_ | _]).

has_move(_, _, _, false).
