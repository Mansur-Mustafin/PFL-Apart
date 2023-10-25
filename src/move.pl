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
    
dfs(CurrRow-CurrCol, NewRow-CurrCol, vertical) :- NewRow is CurrRow + 1.
dfs(CurrRow-CurrCol, NewRow-CurrCol, vertical) :- NewRow is CurrRow - 1.



% get_direction(+CurrPosition, NewPosition, -Direction, -Distance).
get_direction(CurrCol-CurrRow, CurrCol-NewRow, vertical, Distance) :- Distance is abs(CurrRow - NewRow).
get_direction(CurrCol-CurrRow, NewCol-CurrRow, horizontal, Distance) :- Distance is abs(CurrCol - NewCol).
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopLeft, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) > 0,
    Distance is abs(CurrCol - NewCol).
get_direction(CurrCol-CurrRow, NewCol-NewRow, diagTopRight, Distance) :- 
    abs(CurrCol - NewCol) =:= abs(CurrRow - NewRow),
    (NewCol - CurrCol) * (NewRow - CurrRow) < 0,
    Distance is abs(CurrCol - NewCol).
    


% get_number_of_pieces(+Board, +CurrPosCol-CurrPosRow, +Direction, +Player, -Number).
get_number_of_pieces(Board, CurrPosCol-CurrPosRow, horizontal, Player, Number).
    
