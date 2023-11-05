% Verificar outra funcao para dar import sem ser consult

% valid_move(+GameState, ?Position)
valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow) :-
    get_value_at(Board, NewPosRow, NewPosCol, Dest),
    \+ my_piece(Player, Dest),

    \+ member(NewPosCol-NewPosRow, [CurrPosCol-CurrPosRow|T]),

    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, Direction, Distance),
    get_number_of_pieces(Board, CurrPosCol-CurrPosRow, Direction, Player, Distance),
    check_valid_jump([CurrPosCol-CurrPosRow|T], Distance).

check_valid_jump([_ | []], _) :- !.

check_valid_jump([SecondCol-SecondRow, FirstCol-FirstRow], Distance1) :-
    Distance1 > 1, !,
    get_direction(SecondCol-SecondRow, FirstCol-FirstRow, _, Distance),
    Distance > 1.

check_valid_jump([_, _ | _], Distance) :-
    Distance > 1.

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

has_move(Player-NextPlayer, Board, _, false).

explore(SameCol-CurrRow, SameCol-NewRow, vertical, left) :- NewRow is CurrRow - 1.
explore(SameCol-CurrRow, SameCol-NewRow, vertical, right) :- NewRow is CurrRow + 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, left) :- NewCol is CurrCol - 1.
explore(CurrCol-SameRow, NewCol-SameRow, horizontal, right) :- NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, left) :- NewRow is CurrRow - 1, NewCol is CurrCol - 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, right) :- NewRow is CurrRow + 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, left) :- NewRow is CurrRow - 1, NewCol is CurrCol + 1.
explore(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, right) :- NewRow is CurrRow + 1, NewCol is CurrCol - 1.


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
    


% get_number_of_pieces(+Board, +CurrPosCol-CurrPosRow, +Direction, +Player, -Number).
get_number_of_pieces(Board, CurrPosCol-CurrPosRow, Direction, Player, Number) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol1-NewCurrPosRow1, Direction, left),
    explore_end(left, Board, NewCurrPosCol1-NewCurrPosRow1, Direction, Player, N1),
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol2-NewCurrPosRow2, Direction, right),
    explore_end(right, Board, NewCurrPosCol2-NewCurrPosRow2, Direction, Player, N2),
    Number is N1 + N2 + 1.

in_bounds([H | T], CurrPosCol-CurrPosRow) :-
    length([H | T], Rows),
    length(H, Cols),
    CurrPosCol >= 0,
    CurrPosRow >= 0,
    CurrPosRow < Rows,
    CurrPosCol < Cols.

explore_end(_, Board, CurrPosCol-CurrPosRow, _, _, 0) :- \+ in_bounds(Board, CurrPosCol-CurrPosRow), !.
explore_end(_, Board, CurrPosCol-CurrPosRow, _, Player, 0) :- 
    get_value_at(Board, CurrPosRow, CurrPosCol, Piece),
    \+ my_piece(Player, Piece), !.

explore_end(End, Board, CurrPosCol-CurrPosRow, Direction, Player, N) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol-NewCurrPosRow, Direction, End),
    explore_end(End, Board, NewCurrPosCol-NewCurrPosRow, Direction, Player, N1),
    N is N1 + 1, !.

% =====================================================================================
%                                CHECK IF 'X' WINS
% =====================================================================================

% check_win_player(+Board, white).
% true = is the first column?
check_win_player([H1, H2| T], Piece):-
    check_win_player_lists(H1, H2, Piece, true),
    check_win_player([H2|T], Piece).


check_win_player([H1| []], Piece):-
    check_single_row(H1, Piece).


% Checking the first element of the lists
check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, true):-
    \+ ((
        same_piece(H1, H2, Piece);
        same_piece(H1, H4, Piece);
        same_piece(H1, H3, Piece)
    )),
    check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, false).


check_win_player_lists([_, H2, H3|T1], [H4, H5, H6|T2], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece);
        same_piece(H2, H5, Piece);
        same_piece(H2, H6, Piece)
    )),
    check_win_player_lists([H2, H3|T1], [H5, H6|T2], Piece, false).


check_win_player_lists([_, H2|[]], [H3, H4|[]], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece)
    )).


% Just check the single row.
check_single_row([_], _).
check_single_row([H1, H2|T], Piece):-
    \+ same_piece(H1, H2, Piece),
    check_single_row([H2|T], Piece).

% =====================================================================================
%                                FIND WHO WIN
% =====================================================================================

has_won(Board, Player, true):- check_win_player(Board, Player), !.
has_won(_, _, false).

% Cases if one of players wins.
determine_winner(_, true, false, white).
determine_winner(_, false, true, black).

determine_winner(_, false, false, none).

% Aqui aquela regra se os 2 ganharam, quem fez movimento perca.
determine_winner(white, true, true, black).
determine_winner(black, true, true, white).


% =====================================================================================
%                                CASE IF HAS WINNER
% =====================================================================================

print_winner(white):-
    write('[WHITE] The player with white pieces wins!'), nl.

print_winner(black):-
    write('[BLACK] The player with black pieces wins!'), nl.

