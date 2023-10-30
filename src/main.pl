:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').
:- use_module(library(between)).
:- use_module(library(random)).



valid_moves(Player-Board-Visited, Player, ValidMoves) :-
    shape(Board, Rows, Columns),
    Rows1 is Rows - 1,
    Columns1 is Columns - 1,
    setof(OrigCol-OrigRow-NewCol-NewRow, (Player,Board,Visited,Rows1,Columns1)^(
        between(0, Rows1, OrigRow),
        between(0, Columns1, OrigCol),
        between(0, Rows1, NewRow),
        between(0, Columns1, NewCol),
        valid_move(Player-Board-Visited, OrigCol-OrigRow-NewCol-NewRow)
        ), ValidMoves),
    write(ValidMoves).


move(Player-NextPlayer-Board-_, _-_-none-none, NewCurPlayer-NewNextPlayer-Board-[]) :-
    is_human(Player),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer),
    write('Stop'), nl.


move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], 
        CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
        Player-NextPlayer-NewBoard-[NewPosCol-NewPosRow, CurrPosCol-CurrPosRow|T]) :-

    is_human(Player),
    valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow),

    get_value_at(Board, CurrPosRow, CurrPosCol, CurValue),
    
    set_value_at(Board, NewPosRow, NewPosCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard). % Use empty


move(Player-NextPlayer-Board-Visited, _-_-_-_, _-_-_) :-
    is_human(Player),
    write('Move is not valid'), nl,
    game_loop(Player-NextPlayer, Board, Visited).


game_over(Player-Board-_):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    determine_winner(Player, WhiteWins, BlackWins, Winner),
    \+ end_game(Winner).

game_over(_) :-
    write('Do you want play more?'), nl,
    play, !.

% case if we need choose the piece witch will move.
game_loop(Player-NextPlayer, Board, []) :-
    is_human(Player),
    display_game([Player, Board, []]),
    write('Choose witch peice that you want to move'), nl,
    read_pos(OrigColIndex-OriginRowIndex),

    game_loop(Player-NextPlayer, Board, [OrigColIndex-OriginRowIndex]).


game_loop(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T]) :-
    is_human(Player),
    display_game([Player, Board, [CurrPosCol-CurrPosRow|T]]),

    write('Choose in map where you want to go.'), nl,
    read_pos(NewPosCol-NewPosRow),
    
    move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], 
            CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
            NewCurPlayer-NewNextPlayer-NewBoard-NewVisited),

    game_over(Player-NewBoard-NewVisited),
    
    game_loop(NewCurPlayer-NewNextPlayer, NewBoard, NewVisited).


play :-
    display_titel,
    createBoard(Board),
    game_state(Player-NextPlayer, Board, Visited),
    game_loop(Player-NextPlayer, Board, Visited).