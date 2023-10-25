% Verificar outra funcao para dar import sem ser consult
:- consult('state.pl').
:- consult('utils.pl').

% valid_move(+GameState, ?Position)
valid_move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow) :-
    get_value_at(Board, CurrPosRow, CurrPosCol, Orig),
    my_piece(Player, Orig),

    % TODO: verificar que nao vai para uma casa que já visitou
    % TODO: Numero de casa que andou faz sentido?
    % Note: Precisa de gerar os moves possiveis

    get_value_at(Board, NewPosRow, NewPosCol, Dest),
    \+ my_piece(Player, Dest),

    member(Visited, NewPosCol-NewPosRow).
    
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
    explore_end(left, Board, CurrPosCol-CurrPosRow, Direction, Player, N1),
    explore_end(right, Board, CurrPosCol-CurrPosRow, Direction, Player, N2),
    Number is N1 + N2 + 1.

in_bounds([H | T], CurrPosCol-CurrPosRow) :-
    length([H | T], Rows),
    length(H, Cols),
    CurrPosCol >= 0,
    CurrPosRow >= 0,
    CurrPosRow < Rows,
    CurrPosCol < Cols.

explore_end(_, Board, CurrPosCol-CurrPosRow, _, _, -1) :- \+ in_bounds(Board, CurrPosCol-CurrPosRow).
explore_end(_, Board, CurrPosCol-CurrPosRow, _, Player, -1) :- 
    get_value_at(Board, CurrPosRow, CurrPosCol, Piece),
    \+ my_piece(Player, Piece).

explore_end(End, Board, CurrPosCol-CurrPosRow, Direction, Player, N) :-
    explore(CurrPosCol-CurrPosRow, NewCurrPosCol-NewCurrPosRow, Direction, End),
    explore_end(End, Board, NewCurrPosCol-NewCurrPosRow, Direction, Player, N1),
    N is N1 + 1, !.
    

    
