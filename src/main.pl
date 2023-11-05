:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').
:- consult('greedy.pl').
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).

find_all_pieces(Player, Board, ValidPieces) :-
    findall(Col-Row, check_valid_piece(Player, Board, Col-Row), ValidPieces).

valid_moves(Player-_-Board-_, Player, ValidTurns) :-
    \+ is_human(Player),
    find_all_pieces(Player, Board, ValidPieces),
    valid_moves_aux(Player-Board, ValidPieces, [], ValidTurnsAux1),
    maplist(add_end_turn, ValidTurnsAux1, ValidTurnsAux2),
    maplist(reverse, ValidTurnsAux2, ValidTurns).

valid_moves(Player-_-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :-
    is_human(Player),
    get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves).

valid_moves_aux(_-_, [], Answer, Answer). 
valid_moves_aux(Player-Board, [Col-Row | T], Acc, Answer) :-
    set_value_at(Board, Row, Col, empty, NewBoard),
    valid_moves_piece(Player-NewBoard, [[Col-Row]], [], ValidTurns),
    append(ValidTurns, Acc, Acc1),
    valid_moves_aux(Player-Board, T, Acc1, Answer).

valid_moves_piece(_-_, [], ValidTurns, ValidTurns):- !.
valid_moves_piece(Player-Board, CurrMoves, Acc, ValidTurns) :-
    get_next_moves(Player-Board, CurrMoves, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    valid_moves_piece(Player-Board, NewCurrMoves, Acc1, ValidTurns).

get_next_moves(_-_, [], NewMoves, NewMoves).
get_next_moves(Player-Board, [CurrMove | T], Acc, NewMoves) :-
    get_valid_jumps(Player-Board-CurrMove, Player, NextJumps),
    get_next_jumps(CurrMove, NextJumps, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    get_next_moves(Player-Board, T, Acc1, NewMoves).

get_next_jumps(_, [], NewMoves, NewMoves).
get_next_jumps(CurrMove, [CurrJump | T], Acc, NewMoves) :-
    get_next_jumps(CurrMove, T, [[CurrJump | CurrMove] | Acc], NewMoves).

get_valid_jumps(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :- 
    shape(Board, Rows, Columns),
    Rows1 is Rows - 1,
    Columns1 is Columns - 1,

    findall(NewCol-NewRow, (
        between(0, Rows1, NewRow),
        between(0, Columns1, NewCol),
        valid_move(Player-Board-[CurrCol-CurrRow | T], CurrCol-CurrRow-NewCol-NewRow)
        ), ValidMoves).

% Se Primeiro turno e move de distancia 1
check_one_move_turn(Player-NextPlayer-_-[], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-_-[]) :-
    get_direction(CurrPosCol-CurrPosRow, NewPosCol-NewPosRow, _, 1),
    switch_player(Player-NextPlayer, NewCurPlayer-NewNextPlayer).


% Qualquer move sem ser o de cima
check_one_move_turn(Player-NextPlayer-_-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, Player-NextPlayer-_-[NewPosCol-NewPosRow, CurrPosCol-CurrPosRow|T]).

move(Player-NextPlayer-Board-_, _-_-none-none, NewCurPlayer-NewNextPlayer-Board-[]) :-
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

    valid_move(Player-Board-[CurrPosCol-CurrPosRow|T], CurrPosCol-CurrPosRow-NewPosCol-NewPosRow),
    get_value_at(Board, CurrPosRow, CurrPosCol, CurValue),
    set_value_at(Board, NewPosRow, NewPosCol, CurValue, TempBoard),
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard),
    check_one_move_turn(Player-NextPlayer-Board-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-NewBoard-NewVisited).


move(Player-NextPlayer-Board-Visited, _, _) :-
    is_human(Player),
    write('Oops! That move is not valid. Please try again.'), nl,
    game_loop(Player-NextPlayer, Board, Visited).


game_over(Player-_-Board-_, Winner):-
    has_won(Board, white, WhiteWins),
    has_won(Board, black, BlackWins),
    my_piece(Player, Piece),
    determine_winner(Piece, WhiteWins, BlackWins, Winner).

end_game(Board, none, Board) :- !.
end_game(Board, Winner, NewBoard) :-
    display_board(Board),
    print_winner(Winner),
    repeat,
    write('The game has ended. Do you want to play again? (y/n)'), nl,
    peek_char(C), clear_buffer,
    check_replay(Board, C, NewBoard), !.

check_replay(_, y, _) :-
    play.

check_replay(_, n, []) :-
    write('Thank you for playing!'), nl, !.

choose_move(Player-NewPlayer-Board-[], Player, 1, Move) :-
    valid_moves(Player-NewPlayer-Board-[], Player, ValidMoves),
    random_member(Move, ValidMoves).

choose_move(Player-NextPlayer-Board-[], Player, 2, Move) :-
    valid_moves(Player-NextPlayer-Board-[], Player, ValidMoves),
    setof(Value-Move, (ValidMoves, NextPlayer, Board, Player)^(
            member(Move, ValidMoves),
            value(Player-NextPlayer-Board-Move, Player, Value)
        ), EvaluatedMoves),

    write('Line 131'), nl,
    write(EvaluatedMoves), nl,

    reverse(EvaluatedMoves, [BestValue-BestMove| T]), 
    best_turns([BestValue-BestMove | T], BestMoves, BestValue),
    write('Line 136'), nl,
    write(BestMoves), nl,
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

    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),   % verificar isso!

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

    game_over(Player-NextPlayer-NewBoard-NewVisited, Winner),   % verificar isso!

    end_game(NewBoard, Winner, NewBoard1),
    
    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited).

play :-
    display_title,
    initial_state(Player-NextPlayer-Board-Visited),
    game_loop(Player-NextPlayer, Board, Visited).
