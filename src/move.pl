:- ensure_loaded('main.pl').

/*
    File: move.pl
    Description: 
    This file contains all the predicates that deal with the movement of the players
*/

/*
    get_valid_jumps(+Player-Board-Visited, +Player, -ValidMoves)
    Description: get_valid_jumps/3 unifies ValidMoves with all the valid jumps that Player
    can perform from the position they are at currently (the position in the head of the Visited list)
*/
get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :- 
    findall(NewCol-NewRow, (
        valid_move(Player-Board-[CurrCol-CurrRow | T], CurrCol-CurrRow-NewCol-NewRow)
        ), ValidMoves).


/*
    valid_move(+Player-Board-Visited, ?CurrCol-CurrRow-NewCol-NewRow)
    Description: valid_move/2 checks if the move CurrCol-CurrRow to NewCol-NewRow is valid, that is,
    doesn't land on a piece of the player that is performing the move, doesn't land in a position that
    has been visited in the current turn, and moves the correct amount of spaces according to the chosen
    direction
*/
valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow) :-
    get_value_at(Board, NewPosRow, NewPosCol, Dest),
    \+ my_piece(Player, Dest),

    \+ member(NewPosCol-NewPosRow, [CurrPosCol-CurrPosRow|T]),

    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, Direction, Distance),
    get_line_length(Board, CurrPosCol-CurrPosRow, Direction, Player, Distance),
    check_valid_jump([CurrPosCol-CurrPosRow|T], Distance).


% get_direction(+CurrPosition, NewPosition, -Direction, -Distance).
/*
    get_direction(+CurrPosition, +NewPosition, -Direction, -Distance)
    Description: get_direction/4 unifies Direction and Distance with the direction and the number of spaces
    a piece moves from CurrPosition to NewPosition
*/

% Vertical movement
get_direction(SameCol-CurrRow, SameCol-NewRow, vertical, Distance) :- Distance is abs(CurrRow - NewRow).

% Horizontal movement
get_direction(CurrCol-SameRow, NewCol-SameRow, horizontal, Distance) :- Distance is abs(CurrCol - NewCol).

% Diagonal going from left to right and top to bottom
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) > 0,
    Distance is abs(CurrCol - NewCol).

% Diagonal going from right to left and top to bottom
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) < 0,
    Distance is abs(CurrCol - NewCol).


/*
    get_line_length(+Board, +CurrPos, +Direction, +Player, -Number)
    Description: get_line_length/5 unifies Number with the length of the line that is formed by
    a consecutive amount of adjacent pieces from Player that includes a piece in the position CurrPos
*/
get_line_length(Board, CurrPosCol-CurrPosRow, Direction, Player, Number) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol1-NewCurrPosRow1, Direction, left),
    explore_end(left, Board, NewCurrPosCol1-NewCurrPosRow1, Direction, Player, N1),
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol2-NewCurrPosRow2, Direction, right),
    explore_end(right, Board, NewCurrPosCol2-NewCurrPosRow2, Direction, Player, N2),
    Number is N1 + N2 + 1.

/*
    explore(+CurrPos, -NewPos, +Direction, +Way)
    Description: explore/4 calculates the adjacent position to CurrPos according to the Direction
    of the move and the way it follows the Direction resulting in NewPos
*/
explore(SameCol-CurrRow, SameCol-NewRow, vertical, left) :- NewRow is CurrRow - 1.
explore(SameCol-CurrRow, SameCol-NewRow, vertical, right) :- NewRow is CurrRow + 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, left) :- NewCol is CurrCol - 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, right) :- NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, left) :- NewRow is CurrRow - 1, NewCol is CurrCol - 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, right) :- NewRow is CurrRow + 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, left) :- NewRow is CurrRow - 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, right) :- NewRow is CurrRow + 1, NewCol is CurrCol - 1.


/*
    explore_end(+End, +Board, +CurrPos, +Direction, +Player, -N)
    Description: explore_end/6 unifies N with the number of consecutive Player pieces that are found
    by moving towards a certain End of the Direction starting in CurrPos
*/

explore_end(_, Board, CurrPosCol-CurrPosRow, _, _, 0) :- \+ in_bounds(Board, CurrPosCol-CurrPosRow), !.
explore_end(_, Board, CurrPosCol-CurrPosRow, _, Player, 0) :- 
    get_value_at(Board, CurrPosRow, CurrPosCol, Piece),
    \+ my_piece(Player, Piece), !.

explore_end(End, Board, CurrPosCol-CurrPosRow, Direction, Player, N) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol-NewCurrPosRow, Direction, End),
    explore_end(End, Board, NewCurrPosCol-NewCurrPosRow, Direction, Player, N1),
    N is N1 + 1, !.


/*
    check_valid_jump(+Visited, +Distance)
    Description: check_valid_jump/2 check if a jump is valid or not. A jump is always valid if it
    is the first jump of the turn. It isn't valid if the first jump was a move that moved 1 space, or
    if the jump doesn't move more than one space
*/
check_valid_jump([_ | []], _) :- !.

check_valid_jump([SecondCol-SecondRow, FirstCol-FirstRow], Distance1) :-
    Distance1 > 1, !,
    get_direction(SecondCol-SecondRow, FirstCol-FirstRow, _, Distance),
    Distance > 1.

check_valid_jump([_, _ | _], Distance) :-
    Distance > 1.


% Se Primeiro turno e move de distancia 1
/*
    check_one_move_turn(+GameState, +CurrPos-NewPos, -NewGameState)
    Description: check_one_move_turn/3 checks if the player performed a 1 space jump in the first move.
    If so, he cannot move more and the next player will be the next one to play. If not, the current player's turn
    continues
*/

check_one_move_turn(Player-NextPlayer-_-[]-true, _-_-_-_, NewCurPlayer-NewNextPlayer-_-[]-false) :-
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer).

check_one_move_turn(Player-NextPlayer-_-[]-FirstMove, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-_-[]-FirstMove) :-
    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, _, 1),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer).

check_one_move_turn(Player-NextPlayer-_-T-FirstMove, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, Player-NextPlayer-_-[NewPosCol-NewPosRow, CurrPosCol-CurrPosRow|T]-FirstMove).


/*
    find_all_pieces(+Player, +Board, -ValidPieces)
    Description: find_all_pieces/3 unifies ValidPieces with a list with all the pieces that the Player
    can move on its turn, that is, all the pieces that have at least one possible move to be made
*/
find_all_pieces(Player, Board, ValidPieces) :-
    findall(Col-Row, check_valid_piece(Player, Board, Col-Row), ValidPieces).


/*
    valid_moves_aux(+Player-Board, +ValidPieces, +Acc, -Answer)
    Description: recursively calculates all the possible moves for each valid piece in the list
    ValidPieces. The possible moves are stores in Acc and unified with Answer when all pieces
    have been processed
*/
valid_moves_aux(_-_, [], Answer, Answer) :- !. 
valid_moves_aux(Player-Board, [Col-Row | T], Acc, Answer) :-
    set_value_at(Board, Row, Col, empty, NewBoard),
    valid_moves_piece(Player-NewBoard, [[Col-Row]], [], ValidTurns),
    append(ValidTurns, Acc, Acc1),
    valid_moves_aux(Player-Board, T, Acc1, Answer).


/*
    valid_moves_piece(+Player-Board, +CurrMoves, +Acc, -ValidTurns)
    Description: valid_moves_piece/4 unifies ValidTurns with all possible turns to be made
    from the current moves we have 'CurrMoves'. Calculates all possible next moves for each of the 
    moves we calculated previously.
    For example if CurrMoves holds all turns that consist of two different jumps, valid_moves_piece/4
    will calculate all turns that consist of three different jumps from the moves in CurrMoves
*/
valid_moves_piece(_-_, [], ValidTurns, ValidTurns):- !.
valid_moves_piece(Player-Board, CurrMoves, Acc, ValidTurns) :-
    get_next_moves(Player-Board, CurrMoves, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    valid_moves_piece(Player-Board, NewCurrMoves, Acc1, ValidTurns).


/*
    get_next_moves(+Player-Board, +CurrMoves, +Acc, -NewMoves)
    Description: get_next_moves/4 unifies NewMoves with the list of all moves that can be performed
    by starting with each move in CurrMoves
*/
get_next_moves(_-_, [], NewMoves, NewMoves).
get_next_moves(Player-Board, [CurrMove | T], Acc, NewMoves) :-
    get_valid_jumps(Player-Board-CurrMove, Player, NextJumps),
    get_next_jumps(CurrMove, NextJumps, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    get_next_moves(Player-Board, T, Acc1, NewMoves).

/*
    get_next_jumps(+CurrMove, +ValidJumps, +Acc, -NewMoves)
    Description: get_next_jumps/4 unifies NewMoves with a list with all the moves that are formed by appending
    each valid jump from ValidJumps to our CurrMove
*/
get_next_jumps(_, [], NewMoves, NewMoves).
get_next_jumps(CurrMove, [CurrJump | T], Acc, NewMoves) :-
    get_next_jumps(CurrMove, T, [[CurrJump | CurrMove] | Acc], NewMoves).


/*
    check_valid_piece(+Player, +Board, ?Col-Row)
    Description: check_valid_piece/3 checks if the position Col-Row in the Board
    holds a Player piece that has at least one possible move. This rule can be recursively
    called to get all possible valid pieces for Player
*/
check_valid_piece(Player, Board, Col-Row) :-
    get_value_at(Board, Row, Col, Value),
    my_piece(Player, Value),
    get_valid_jumps(Player-Board-[Col-Row], Player, [_ | _]).


/*
    valid_piece_choice(+Player-NextPlayer, +Board, Pos)
    Description: valid_piece_choice checks if the piece chosen by Player in position Pos is valid or not.
    If it is we let the player continue its turn by moving the piece. If not, he will have to pick the piece again
*/
valid_piece_choice(Player-NextPlayer, Board, Col-Row, FirstMove):-
    \+ is_none(Col),
    check_valid_piece(Player, Board, Col-Row),
    game_loop(Player-NextPlayer, Board, [Col-Row], FirstMove).

valid_piece_choice(Player-NextPlayer, Board, _, FirstMove) :-
    write('Please choose again.'), nl,
    game_loop(Player-NextPlayer, Board, [], FirstMove).


/*
    has_move(+Player-NextPlayer, +Board, +Visited, -HasMove)
    Descripton: has_move/4 unifies HasMove with true if there is a possible move from the current position
    of the Player current piece (whose position is the head of the list Visited). Unifies HasMove with false otherwise
*/
has_move(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T], true) :-
    valid_moves(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], Player, [_ | _]).

has_move(_, _, _, false).
