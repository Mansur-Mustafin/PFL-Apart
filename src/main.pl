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

% display_game(+Game_State)
display_game(CurrentPlayer-_-Board-Visited) :-
    display_player(CurrentPlayer),
    show_valid_moves(CurrentPlayer, Board, Visited, NewBoard1),
    process_visited(NewBoard1, Visited, true, NewBoard2),
    display_board(NewBoard2).

initial_state(FirstPlayer-SecondPlayer-Board-[]):-
    get_mode(Lvl),
    createPlayer(Lvl, TempFirstPlayer-TempSecondPlayer),
    choose_computer(TempFirstPlayer, white, FirstPlayer),
    choose_computer(TempSecondPlayer, black, SecondPlayer),
    get_board_size(NumCol-NumRow),
    createBoard(NumCol-NumRow, Board).

move(Player-NextPlayer-Board-[_, _ | _], _-_-none-none, NewCurPlayer-NewNextPlayer-Board-[]) :-
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer),
    write('You chose to stop your movement. It\'s the next player\'s turn now.'), nl.

move(Player-NextPlayer-Board-[CurrCol-CurrRow, NextCol-NextRow | T],
        CurrCol-CurrRow-NextCol-NextRow,
        Player-NextPlayer-NewBoard-[NextCol-NextRow | T]) :-
    \+ is_human(Player),

    get_value_at(Board, CurrRow, CurrCol, CurValue),
    set_value_at(Board, NextRow, NextCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrRow, CurrCol, empty, NewBoard).

move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], 
        CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
        NewCurPlayer-NewNextPlayer-NewBoard-NewVisited) :-
    is_human(Player),
    \+ is_none(NewPosCol),
    \+ is_none(NewPosRow),

    valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow),
    get_value_at(Board, CurrPosRow, CurrPosCol, CurValue),
    set_value_at(Board, NewPosRow, NewPosCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard),
    check_one_move_turn(Player-NextPlayer-Board-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-NewBoard-NewVisited).


move(Player-NextPlayer-Board-Visited, _, _) :-
    is_human(Player),
    write('Oops! That move is not valid. Please try again.'), nl,
    game_loop(Player-NextPlayer, Board, Visited).

valid_moves(Player-_-Board-_, Player, ValidTurns) :-
    \+ is_human(Player),
    find_all_pieces(Player, Board, ValidPieces),
    valid_moves_aux(Player-Board, ValidPieces, [], ValidTurnsAux1),
    maplist(add_end_turn, ValidTurnsAux1, ValidTurnsAux2),
    maplist(reverse, ValidTurnsAux2, ValidTurns).

valid_moves(Player-_-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :-
    is_human(Player),
    get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves).

game_over(Player-_-Board-_, Winner):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    my_piece(Player, Piece),
    determine_winner(Piece, WhiteWins, BlackWins, Winner).

% value(+GameState, +Player, -Value)
value(Player-_-Board-[FirstCol-FirstRow | T], Player, Value) :-
    get_number_of_separate_pieces(Board, Player, Nbefore),
    set_value_at(Board, FirstRow, FirstCol, empty, NewBoard),
    process_turn(T, NewBoard, Player, InfluenceRate, NewBoard2), !,
    get_number_of_separate_pieces(NewBoard2, Player, Nafter),

    Value is Nafter - Nbefore - InfluenceRate.

choose_move(Player-NewPlayer-Board-[], Player, 1, Move) :-
    valid_moves(Player-NewPlayer-Board-[], Player, ValidMoves),
    random_member(Move, ValidMoves).

choose_move(Player-NextPlayer-Board-[], Player, 2, Move) :-
    valid_moves(Player-NextPlayer-Board-[], Player, ValidMoves),
    setof(Value-Move, (ValidMoves, NextPlayer, Board, Player)^(
            member(Move, ValidMoves),
            value(Player-NextPlayer-Board-Move, Player, Value)
        ), EvaluatedMoves),

    reverse(EvaluatedMoves, [BestValue-BestMove| T]), 
    best_turns([BestValue-BestMove | T], BestMoves, BestValue),
    random_member(Move, BestMoves).

game_loop(_-_, [], _).

game_loop(Player-NextPlayer, Board, []) :-
    is_human(Player),
    display_game(Player-NextPlayer-Board-[]),
    write('Please select the piece you wish to move.'), nl,
    read_pos(OrigColIndex-OriginRowIndex, true),
    valid_piece_choice(Player-NextPlayer, Board, OrigColIndex-OriginRowIndex).

game_loop(Player-NextPlayer, Board, []) :-
    \+ is_human(Player),
    level(Player, Level),
    choose_move(Player-NextPlayer-Board-[], Player, Level, Turn),
    game_loop(Player-NextPlayer, Board, Turn).

game_loop(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T]) :-
    is_human(Player),

    display_game(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T]),

    has_move(Player-NextPlayer, Board, [CurrPosCol-CurrPosRow|T], HasMove),

    write('Now, choose your destination on the board.'), nl,
    read_pos(NewPosCol-NewPosRow, HasMove),
    
    move(Player-NextPlayer-Board-[CurrPosCol-CurrPosRow|T], 
            CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, 
            NewCurPlayer-NewNextPlayer-NewBoard-NewVisited),

    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),

    end_game(NewBoard, Winner, NewBoard1),

    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited).

game_loop(Player-NextPlayer, Board, [CurrCol-CurrRow, NextCol-NextRow| T]) :-
    \+ is_human(Player),

    move(Player-NextPlayer-Board-[CurrCol-CurrRow, NextCol-NextRow | T],
            CurrCol-CurrRow-NextCol-NextRow, 
            NewCurPlayer-NewNextPlayer-NewBoard-NewVisited),

    display_game(Player-NextPlayer-NewBoard-[]),
    display_pc_move(Player, CurrCol-CurrRow, NextCol-NextRow),

    nl, write('Enter anything to continue: '), nl,

    peek_char(_), clear_buffer,

    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),

    end_game(NewBoard, Winner, NewBoard1),
    
    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited).

play :-
    display_title,
    initial_state(Player-NextPlayer-Board-Visited),
    game_loop(Player-NextPlayer, Board, Visited).
