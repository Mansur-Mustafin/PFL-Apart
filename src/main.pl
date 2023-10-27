:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').

valid_moves(Player-Board-Visited, Player, ValidMoves) :-
    write('Todo'), nl.

move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited) :-
    % vlaid_move,
    % mover peca
    write('Todo'), nl.

% If no winners so Winner => fali, but we can do Winner = none
game_over(Player-Board-Visited, Winner):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    determine_winner(Player, WhiteWins, BlackWins, Winner),
    \+ end_game(Winner).


game_loop(Player, Board, Visited) :-
    display_game([Player, Board, Visited]),
    valid_moves(Player-Board-Visited, Player, ValidMoves),
    % Change this block of code to get a move ------------------------------------------------------
    write('Enter origin coordinates (Col-Row): '), nl,
    read(OriginCol-OriginRow),
    get_index(OriginCol, OrigColIndex), 

    % Ask user for destination coordinates
    write('Enter destination coordinates (Col-Row): '), nl,
    read(DestCol-DestRow),
    get_index(DestCol, DestColIndex),

    OriginRowIndex is OriginRow - 1,
    DestRowIndex is DestRow - 1,

    % valid_move(Player-Board-Visited, OrigColIndex-OriginRowIndex-DestColIndex-DestRowIndex),

    % Call step predicate with user input
    step(Board, OrigColIndex-OriginRowIndex, DestColIndex-DestRowIndex, NewBoard),

    % ------------------------------------------------------------------------------------------

    % move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited),
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
	game_state(Player, Board, Visited),
    game_loop(Player, Board, Visited).