:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('computer.pl').
:- consult('game_over.pl').
:- consult('io.pl').
:- consult('move.pl').
:- consult('state.pl').
:- consult('utils.pl').
:- consult('view.pl').

/*
    File: main.pl
    Description: 
    This file contains all the main predicates of this project, that is, all predicates
    that were mandatory to have according to the project's specification and the game loops.
*/

% display_game(+Game_State)
/*
    display_game(+GameState)
    Description: display_game/1 displays the player that is playing, and the board with
    the valid moves the player can perform, the positions that have already been visited and all
    the pieces in the board
*/
display_game(CurrentPlayer-_-Board-Visited) :-
    display_player(CurrentPlayer),
    show_valid_moves(CurrentPlayer, Board, Visited, NewBoard1),
    process_visited(NewBoard1, Visited, true, NewBoard2),
    display_board(NewBoard2).


/*
    initial_state(-GameState)

*/
initial_state(FirstPlayer-SecondPlayer-Board-[]):-
    get_mode(Lvl),
    createPlayer(Lvl, TempFirstPlayer-TempSecondPlayer),
    choose_computer(TempFirstPlayer, white, FirstPlayer),
    choose_computer(TempSecondPlayer, black, SecondPlayer),
    get_board_size(NumCol-NumRow),
    createBoard(NumCol-NumRow, Board).


/*
     move(+GameState, +Move, -NewGameState)
     Description: move/3 validates and performs the Move unifying NewGameState with the GameState after the move.
*/

% Case where the player has chosen to stop moving
move(Player-NextPlayer-Board-[_, _ | _]-_, _-_-none-none, NewCurPlayer-NewNextPlayer-Board-[]-false) :-
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer),
    write('You chose to stop your movement. It\'s the next player\'s turn now.'), nl.

% Case where the player is a computer
move(Player-NextPlayer-Board-[CurrCol-CurrRow, NextCol-NextRow | T]-FirstMove,
        CurrCol-CurrRow-NextCol-NextRow,
        Player-NextPlayer-NewBoard-[NextCol-NextRow | T]-FirstMove) :-
    \+ is_human(Player),

    get_value_at(Board, CurrRow, CurrCol, CurValue),
    set_value_at(Board, NextRow, NextCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrRow, CurrCol, empty, NewBoard).

% Case where the player is a human
move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T]-FirstMove, 
        CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
        NewCurPlayer-NewNextPlayer-NewBoard-NewVisited-NewFirstMove) :-
    is_human(Player),
    \+ is_none(NewPosCol),
    \+ is_none(NewPosRow),

    valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow),
    get_value_at(Board, CurrPosRow, CurrPosCol, CurValue),
    set_value_at(Board, NewPosRow, NewPosCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard),
    check_one_move_turn(Player-NextPlayer-Board-T-FirstMove, 
                        CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
                        NewCurPlayer-NewNextPlayer-NewBoard-NewVisited-NewFirstMove).

% Case where the move that was input is not valid
move(Player-NextPlayer-Board-Visited-FirstMove, _, _) :-
    is_human(Player),
    write('Oops! That move is not valid. Please try again.'), nl,
    game_loop(Player-NextPlayer, Board, Visited, FirstMove).


/*
    valid_moves(+GameState, +Player, -ListOfMoves)
    Description: valid_moves/3 unifies ListOfMoves with a list containing all possible 
    moves for a player. In case of a computer a move with be a sequence of jumps that represents
    its entire turn. In case of a human, a move is a single jump in its turn that may contain various jumps
*/
valid_moves(Player-_-Board-_, Player, ValidTurns) :-
    \+ is_human(Player),
    find_all_pieces(Player, Board, ValidPieces),
    valid_moves_aux(Player-Board, ValidPieces, [], ValidTurnsAux1),
    maplist(add_end_turn, ValidTurnsAux1, ValidTurnsAux2),
    maplist(reverse, ValidTurnsAux2, ValidTurns).

valid_moves(Player-_-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :-
    is_human(Player),
    get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves).


/*
    game_over(+GameState, -Winner)
    Description: game_over/2 verifies if the game has ended. If that is the case, Winner is unified with
    the player that one the game. If no player won, Winner is unified with 'none'
*/
game_over(Player-_-Board-_, Winner):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    my_piece(Player, Piece),
    determine_winner(Piece, WhiteWins, BlackWins, Winner).


/*
    value(+GameState, +Player, -Value)
    Description: value/3 determines how good or bad a move chosen by the computer is, associating
    it with a value. The greater the value is, the better the move is considered. The calculation of the value
    takes into account the difference between the number of separate pieces before and after the move, the
    amount of enemy pieces that were captured and the amount of separate groups of adjacent enemy pieces that
    were formed by eating enemy pieces
*/
value(Player-_-Board-[FirstCol-FirstRow | T], Player, Value) :-
    get_number_of_separate_pieces(Board, Player, Nbefore),
    set_value_at(Board, FirstRow, FirstCol, empty, NewBoard),
    process_turn(T, NewBoard, Player, InfluenceRate, NewBoard2), !,
    get_number_of_separate_pieces(NewBoard2, Player, Nafter),

    Value is Nafter - Nbefore - InfluenceRate.


/*
    choose_move(+GameState, +Player, +Level, -Move)
    Description: choose_move/4 unifies Move with a valid move that is selected by the computer.
    In the case of the random computer he chooses a random valid movement. In the case of the greedy
    computer he chooses the random valid movement from the ones that have the highest value associated to it
*/

% Random computer
choose_move(Player-NewPlayer-Board-[], Player, 1, Move) :-
    valid_moves(Player-NewPlayer-Board-[], Player, ValidMoves),
    random_member(Move, ValidMoves).

% Greedy computer
choose_move(Player-NextPlayer-Board-[], Player, 2, Move) :-
    valid_moves(Player-NextPlayer-Board-[], Player, ValidMoves),
    setof(Value-Move, (ValidMoves, NextPlayer, Board, Player)^(
            member(Move, ValidMoves),
            value(Player-NextPlayer-Board-Move, Player, Value)
        ), EvaluatedMoves),

    reverse(EvaluatedMoves, [BestValue-BestMove| T]), 
    best_turns([BestValue-BestMove | T], BestMoves, BestValue),
    random_member(Move, BestMoves).

/*
    game_loop(+Player-NextPlayer, +Board, +Visited)
    Description: game_loop/3 determines the actions to take for each player in each state of the game
    (choosing a piece, moving a piece or ending the game).
*/

% Game has ended and user doesn't want to play more
game_loop(_-_, [], _, _).

% Human player chooses its next move
game_loop(Player-NextPlayer, Board, [], FirstMove) :-
    is_human(Player),
    display_game(Player-NextPlayer-Board-[]),
    write('Please select the piece you wish to move.'), nl,
    read_pos(OrigColIndex-OriginRowIndex, true),
    valid_piece_choice(Player-NextPlayer, Board, OrigColIndex-OriginRowIndex, FirstMove).

% Computer player chooses its next move
game_loop(Player-NextPlayer, Board, [], FirstMove) :-
    \+ is_human(Player),
    level(Player, Level),
    choose_move(Player-NextPlayer-Board-[], Player, Level, Turn),
    game_loop(Player-NextPlayer, Board, Turn, FirstMove).

% Human player performs its move
game_loop(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T], FirstMove) :-
    is_human(Player),

    display_game(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T]),

    has_move(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T], HasMove),

    write('Now, choose your destination on the board.'), nl,
    read_pos(NewPosCol-NewPosRow, HasMove),
    
    write('195'), nl,

    move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T]-FirstMove, 
            CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
            NewCurPlayer-NewNextPlayer-NewBoard-NewVisited-NewFirstMove),

    write('201'), nl,


    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),

    end_game(NewBoard, Winner, NewBoard1),

    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited, NewFirstMove).

% Computer player performs its move
game_loop(Player-NextPlayer, Board, [CurrCol-CurrRow, NextCol-NextRow| T], FirstMove) :-
    \+ is_human(Player),

    move(Player-NextPlayer-Board-[CurrCol-CurrRow, NextCol-NextRow | T]-FirstMove,
            CurrCol-CurrRow-NextCol-NextRow, 
            NewCurPlayer-NewNextPlayer-NewBoard-NewVisited-NewFirstMove),

    display_game(Player-NextPlayer-NewBoard-[]),
    display_pc_move(Player, CurrCol-CurrRow, NextCol-NextRow),

    nl, write('Enter anything to continue: '), nl,

    peek_char(_), clear_buffer,

    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),

    end_game(NewBoard, Winner, NewBoard1),
    
    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited, NewFirstMove).


/*
    Description: play/0 is the entrypoint of the program and defines the initial game state, calling
    the first game loop of the game
*/
play :-
    display_title,
    initial_state(Player-NextPlayer-Board-Visited),
    game_loop(Player-NextPlayer, Board, Visited, true).
