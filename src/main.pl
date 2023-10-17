:- consult('view.pl').
:- consult('state.pl').

play :-
	display_titel,
	createBoard(Board),
	game_state(Player, Board, Visited),
	display_game([Player, Board, Visited]),

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
    
    % Display the new board state
    display_game([Player, NewBoard, Visited]).