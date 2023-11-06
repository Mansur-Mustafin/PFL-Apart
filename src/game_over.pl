:- ensure_loaded('main.pl').

/*
    File: game_over.pl
    Description: 
    This file contain all predicates essential for managing game logic in 
    determining win conditions. 
*/


/*
    has_won(+Board, +Player, -Flag).
    Description: Checks if the given Player has won on the Board and sets Flag to 'true' or 'false'.
*/
has_won(Board, Player, true):- check_win_player(Board, Player), !.
has_won(_, _, false).


/*
    check_win_player/2
    Description: Verifies if a piece occupies any three consecutive positions 
                either vertically, horizontally, or diagonally on the Board. The main idea is to:
                check each pair of lists for uninterrupted sequences of pieces.
*/
% Recursive case: Check two adjacent lists for contiguous pieces
check_win_player([H1, H2| T], Piece):-
    check_win_player_lists(H1, H2, Piece, true),
    check_win_player([H2|T], Piece).

% Base case: If the list is the last one on the Board, check only that.
check_win_player([H1| []], Piece):-
    check_single_row(H1, Piece).


/*
    check_win_player_lists(+FirstList, +SecondList, Piece, Flag).
    Description: This predicate checks two adjacent rows (FirstList and SecondList) 
                    for consecutive pieces.
    Flag: is used to differentiate the first element check (true) from the rest (false).
*/
% Checking the first element of the lists for a winning pattern.
check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, true):-
    \+ ((
        same_piece(H1, H2, Piece);
        same_piece(H1, H4, Piece);
        same_piece(H1, H3, Piece)
    )),
    check_win_player_lists([H1, H2|T1], [H3, H4|T2], Piece, false).

% Checking the elements not at the boundaries of the lists for a winning pattern.
check_win_player_lists([_, H2, H3|T1], [H4, H5, H6|T2], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece);
        same_piece(H2, H5, Piece);
        same_piece(H2, H6, Piece)
    )),
    check_win_player_lists([H2, H3|T1], [H5, H6|T2], Piece, false).

% Checking the last element of the lists for a winning pattern.
check_win_player_lists([_, H2|[]], [H3, H4|[]], Piece, false):-
    \+ ((
        same_piece(H2, H3, Piece);
        same_piece(H2, H4, Piece)
    )).

/*
    check_single_row(+List, +Piece).
    Description: Checks if a single row (List) does not contain a sequence of a given Piece.
*/
check_single_row([_], _).

check_single_row([H1, H2|T], Piece):-
    \+ same_piece(H1, H2, Piece),
    check_single_row([H2|T], Piece).


/*
    determine_winner(+Piece, +FirstPlayerWon, +SecondPlayerWon, -Winner).
    Description: determine_winner/4 Determines the game's winner based on the victory status of each player.
*/
determine_winner(_, true, false, white).
determine_winner(_, false, true, black).

determine_winner(_, false, false, none).

% Case where both players have a winning sequence.
determine_winner(white, true, true, black).
determine_winner(black, true, true, white).


/*
    end_game(+Board, +Winner, -NewBoard).
    Description: end_game/3 Handles the end of the game by displaying the final board, 
                announcing the winner, and asking play again or exit.
*/
end_game(Board, none, Board) :- !.

end_game(Board, Winner, NewBoard) :-
    display_board(Board),
    print_winner(Winner),
    repeat,
    write('The game has ended. Do you want to play again? (y/n)'), nl,
    peek_char(C), clear_buffer,
    check_replay(C, NewBoard), !.


/*
    check_replay(+Desision, -NewBoard).
    Description: check_replay/2 Determines the NewBoard based on the player's decision,
                 unifyed NewBoard with empty list to stop the all game (handled in game_loop).
*/
check_replay(y, _) :-
    play.

check_replay(n, []) :-
    write('Thank you for playing!'), nl, !.
