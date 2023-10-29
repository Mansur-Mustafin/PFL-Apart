:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').
:- use_module(library(between)).
:- use_module(library(random)).


read_move(Player-_-_, OrigColIndex-OriginRowIndex-DestColIndex-DestRowIndex) :-
    is_human(Player),
    read_pos(OrigColIndex-OriginRowIndex),
    read_pos(DestColIndex-DestRowIndex).

read_move(Player-Board-Visited, OrigColIndex-OriginRowIndex-DestColIndex-DestRowIndex) :-
    \+ is_human(Player),
    valid_moves(Player-Board-Visited, Player, ValidMoves),
    random_member(OrigColIndex-OriginRowIndex-DestColIndex-DestRowIndex, ValidMoves).



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


move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited) :-
    % Se move não igual a acabar
    valid_move()
    get_value_at(Board, OriginRow, OriginCol, OriginValue),
    OriginValue \= empty, %TODO: check the player.
    get_value_at(Board, DestRow, DestCol, DestValue),
    DestValue = empty, !,
    set_value_at(Board, DestRow, DestCol, OriginValue, TempBoard),
    set_value_at(TempBoard, OriginRow, OriginCol, DestValue, NewBoard).

move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited) :-
    % Se move igual a acabar
    valid_move()
    get_value_at(Board, OriginRow, OriginCol, OriginValue),
    OriginValue \= empty, %TODO: check the player.
    get_value_at(Board, DestRow, DestCol, DestValue),
    DestValue = empty, !,
    set_value_at(Board, DestRow, DestCol, OriginValue, TempBoard),
    set_value_at(TempBoard, OriginRow, OriginCol, DestValue, NewBoard).


% If no winners so Winner => fali, but we can do Winner = none
game_over(Player-Board-Visited, Winner):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    determine_winner(Player, WhiteWins, BlackWins, Winner),
    \+ end_game(Winner).


game_loop(Player-NextPlayer, Board, Visited) :-
    display_game([Player, Board, Visited]),

    read_move(Player, OrigColIndex-OriginRowIndex-DestColIndex-DestRowIndex),

    % Call step predicate with user input
    step(Board, OrigColIndex-OriginRowIndex, DestColIndex-DestRowIndex, NewBoard),

    % ------------------------------------------------------------------------------------------

    move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited),
    % change player if visited is empty?
    game_over(Player-NewBoard-Visited, Winner), write('line 47'), nl,
    game_loop(Player, NewBoard, Visited).   % To test
    % game_loop(NewPlayer, NewBoard, NewVisited).

game_over(_ , _) :-
    write('Do you want play more?'), nl,
    play, !.

play :-
    display_titel,
    createBoard(Board),
    game_state(Player-NextPlayer, Board, Visited),
    game_loop(Player-NextPlayer, Board, Visited).