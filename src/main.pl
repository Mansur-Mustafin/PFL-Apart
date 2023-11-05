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

valid_turns_robot(Player-Board, Player, ValidTurns) :-
    find_all_pieces(Player, Board, ValidPieces),
    valid_turns_aux(Player-Board, ValidPieces, [], ValidTurnsAux1),
    maplist(add_end_turn, ValidTurnsAux1, ValidTurnsAux2),
    maplist(reverse, ValidTurnsAux2, ValidTurns).

valid_turns_aux(_-_, [], Answer, Answer). 
valid_turns_aux(Player-Board, [Col-Row | T], Acc, Answer) :-
    set_value_at(Board, Row, Col, empty, NewBoard),
    valid_turns_piece(Player-NewBoard, [[Col-Row]], [], ValidTurns),
    append(ValidTurns, Acc, Acc1),
    valid_turns_aux(Player-Board, T, Acc1, Answer).

valid_turns_piece(_-_, [], ValidTurns, ValidTurns):- !.
valid_turns_piece(Player-Board, CurrMoves, Acc, ValidTurns) :-
    get_next_moves(Player-Board, CurrMoves, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    valid_turns_piece(Player-Board, NewCurrMoves, Acc1, ValidTurns).

get_next_moves(_-_, [], NewMoves, NewMoves).
get_next_moves(Player-Board, [CurrMove | T], Acc, NewMoves) :-
    valid_moves_player(Player-Board-CurrMove, Player, NextJumps),
    get_next_jumps(CurrMove, NextJumps, [], NewCurrMoves),
    append(NewCurrMoves, Acc, Acc1),
    get_next_moves(Player-Board, T, Acc1, NewMoves).

get_next_jumps(CurrMove, [], NewMoves, NewMoves).
get_next_jumps(CurrMove, [CurrJump | T], Acc, NewMoves) :-
    get_next_jumps(CurrMove, T, [[CurrJump | CurrMove] | Acc], NewMoves).

valid_moves_player(Player-Board-[CurrCol-CurrRow | T], Player, ValidMoves) :-
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
    set_value_at(TempBoard, CurrPosRow, CurrPosCol, empty, NewBoard), % Use empty
    check_one_move_turn(Player-NextPlayer-Board-T, CurrPosCol-CurrPosRow-NewPosCol-NewPosRow, NewCurPlayer-NewNextPlayer-NewBoard-NewVisited).


move(Player-NextPlayer-Board-Visited, _, _) :-
    is_human(Player),
    write('Oops! That move is not valid. Please try again.'), nl,
    game_loop(Player-NextPlayer, Board, Visited).


game_over(Player-Board-_, Winner):-
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

choose_move(Player-_-Board-[], Player, 1, Turn) :-
    valid_turns_robot(Player-Board, Player, ValidTurns),
    random_member(Turn, ValidTurns).

choose_move(Player-NextPlayer-Board-[], Player, 2, Turn) :-
    valid_turns_robot(Player-Board, Player, ValidTurns),
    setof(Value-Turn, (ValidTurns, NextPlayer, Board, Turn, Player)^(
            member(Turn, ValidTurns),
            value(Player-NextPlayer-Board-Turn, Player, Value)
        ), EvaluatedTurns),

    reverse(EvaluatedTurns, [BestValue-BestTurn| T]), 
    best_turns([BestValue-BestTurn | T], BestTurns, BestValue), 
    random_member(Turn, BestTurns).

game_loop(_-_, [], _).

game_loop(Player-NextPlayer, Board, []) :-
    is_human(Player),
    display_game(Player-NextPlayer-Board-[]),
    write('Please select the piece you wish to move.'), nl,
    read_pos(OrigColIndex-OriginRowIndex, true),
    valid_piece_choice(Player-NextPlayer, Board, OrigColIndex-OriginRowIndex). % TODO: check if user selected the right piece.

game_loop(Player-NextPlayer, Board, []) :-
    % is_easy_pc(Player),
    \+ is_human(Player),
    % valid_turns_robot(Player-Board, Player, ValidTurns),
    level(Player, Level),
    choose_move(Player-NextPlayer-Board-[], Player, Level, Turn),
    %random_member(Turn, ValidTurns),
    write(Turn), nl,
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

    game_over(Player-NewBoard-NewVisited, Winner),

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

    game_over(Player-NewBoard-NewVisited, Winner),

    end_game(NewBoard, Winner, NewBoard1),
    
    game_loop(NewCurPlayer-NewNextPlayer, NewBoard1, NewVisited).

play :-
    display_titel,
    createBoard(Board),
    game_state(Player-NextPlayer, Board, Visited),
    game_loop(Player-NextPlayer, Board, Visited).
