:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).


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


% Se Primeiro turno e move de distancia 1
check_one_move_turn(Player-NextPlayer-_-[], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-_-[]) :-
    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, _, 1),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer).


% Qualquer move sem ser o de cima
check_one_move_turn(Player-NextPlayer-_-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, Player-NextPlayer-_-[NewPosCol-NewPosRow, CurrPosCol-CurrPosRow|T]).


move(Player-NextPlayer-Board-_, _-_-none-none, NewCurPlayer-NewNextPlayer-Board-[]) :-
    is_human(Player),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer),
    write('You chose to stop your movement. It\'s the next player\'s turn now.'), nl.


move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], 
        CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
        NewCurPlayer-NewNextPlayer-NewBoard-NewVisited) :-

    is_human(Player),
    valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow),

    get_value_at(Board, CurrPosRow, CurrPosCol, CurValue),
    
    set_value_at(Board, NewPosRow, NewPosCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard), % Use empty
    check_one_move_turn(Player-NextPlayer-Board-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-NewBoard-NewVisited).


move(Player-NextPlayer-Board-Visited, _, _) :-
    is_human(Player),
    write('Oops! That move is not valid. Please try again.'), nl,
    game_loop(Player-NextPlayer, Board, Visited).


game_over(Player-Board-_):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    determine_winner(Player, WhiteWins, BlackWins, Winner),
    \+ end_game(Winner).

game_over(_) :-
    write('The game has ended. Do you want to play again?'), nl,
    play, !.

% case if we need choose the piece witch will move.
game_loop(Player-NextPlayer, Board, []) :-
    is_human(Player),
    display_game([Player, Board, []]),
    write('Please select the piece you wish to move.'), nl,
    read_pos(OrigColIndex-OriginRowIndex),

    valid_piece_choice(Player-NextPlayer, Board, OrigColIndex-OriginRowIndex). % TODO: check if user selected the right piece.


game_loop(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T]) :-
    is_human(Player),
    display_game([Player, Board, [CurrPosCol-CurrPosRow|T]]),

    write('Now, choose your destination on the board.'), nl,
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