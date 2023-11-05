:- ensure_loaded('main.pl').

has_won(Board, Player, true):- check_win_player(Board, Player), !.
has_won(_, _, false).


% check_win_player(+Board, white).
% true = is the first column?
check_win_player([H1, H2| T], Piece):-
    check_win_player_lists(H1, H2, Piece, true),
    check_win_player([H2|T], Piece).


check_win_player([H1| []], Piece):-
    check_single_row(H1, Piece).


% Checking the first element of the lists
check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, true):-
    \+ ((
        same_piece(H1, H2, Piece);
        same_piece(H1, H4, Piece);
        same_piece(H1, H3, Piece)
    )),
    check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, false).


check_win_player_lists([_, H2, H3|T1], [H4, H5, H6|T2], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece);
        same_piece(H2, H5, Piece);
        same_piece(H2, H6, Piece)
    )),
    check_win_player_lists([H2, H3|T1], [H5, H6|T2], Piece, false).


check_win_player_lists([_, H2|[]], [H3, H4|[]], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece)
    )).


% Just check the single row.
check_single_row([_], _).
check_single_row([H1, H2|T], Piece):-
    \+ same_piece(H1, H2, Piece),
    check_single_row([H2|T], Piece).


% Cases if one of players wins.
determine_winner(_, true, false, white).
determine_winner(_, false, true, black).

determine_winner(_, false, false, none).

% Aqui aquela regra se os 2 ganharam, quem fez movimento perca.
determine_winner(white, true, true, black).
determine_winner(black, true, true, white).


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
