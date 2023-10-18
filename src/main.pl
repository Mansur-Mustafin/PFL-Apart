:- consult('view.pl').
:- consult('state.pl').

valid_moves(Player-Board-Visited, Player, ValidMoves) :-
    write('Todo'), nl.

move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited) :-
    write('Todo'), nl.

game_over(Player-Board-Visited, Winner) :-
    write('Todo'), nl.

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
    % Call step predicate with user input
    step(Board, OrigColIndex-OriginRowIndex, DestColIndex-DestRowIndex, NewBoard),

    % ------------------------------------------------------------------------------------------

    move(Player-Board-Visited, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewPlayer-NewBoard-NewVisited),
    % change player if visited is empty?
    game_over(Player-Board-Visited, Winner),
    game_loop(NewPlayer, NewBoard, NewVisited).


play :-
	display_titel,
	createBoard(Board),
	game_state(Player, Board, Visited),
    game_loop(Player, Board, Visited).