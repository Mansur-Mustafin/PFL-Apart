% Vamos aqui escrever pequenos testes de funcoes.
:- consult('view.pl').
:- consult('state.pl').
:- consult('move.pl').
:- consult('utils.pl').

get_board1(Board) :-
    Board = [   [empty, black, black, black, empty], % 0
                [empty, black, black, black, empty], % 1
                [empty, empty, empty, empty, empty], % 2
                [empty, empty, empty, empty, empty], % 3
                [empty, empty, empty, empty, empty], % 4
                [empty, white, empty, empty, empty], % 5
                [empty, empty, empty, empty, empty], % 6
                [empty, white, white, white, empty], % 7
                [empty, white, white, white, empty]  % 8
    ].

get_board2(Board) :-
    Board = [   
                [empty, empty, empty, empty],
                [empty, white, white, empty]
    ].


get_board3(Board) :-
    Board = [   [empty, black, black, black, black, black, black, black, black, empty], % 0
                [empty, black, black, black, black, black, black, black, black, empty], % 1
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 2
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 3
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 4
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 5
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 6
                [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty], % 7
                [empty, white, white, white, white, white, white, white, white, empty], % 8
                [empty, white, white, white, white, white, white, white, white, empty]  % 9
    ].

get_board4(Board) :-
    Board = [   [white, empty, white], 
                [empty, white, empty], 
                [white, empty, white]
    ].

test_check_win_player:-
    % Test 1: No win state.
    Board1 = [	[white, black, white], 
    			[black, white, black], 
    			[white, black, white]
    		],
    \+ check_win_player(Board1, white),
    \+ check_win_player(Board1, black),
    write('Test 1 passed'), nl, nl,

    % Test 2: White wins with pieces separated.
    Board2 = [	[empty, white, empty], 
    			[white, empty, white], 
    			[empty, white, empty]
    		],
    \+ check_win_player(Board2, white),
    check_win_player(Board2, black),
	write('Test 2 passed'), nl, nl,

    % Test 3: Black wins with pieces separated.
    Board3 = [	[empty, black, empty], 
    			[black, empty, black], 
    			[empty, black, empty]
    		],
    check_win_player(Board3, white),
    \+ check_win_player(Board3, black),
	write('Test 3 passed'), nl, nl,

    % Test 4: Both players have pieces separated but White wins because Black moved last.
    Board4 = [	[empty, white, empty], 
    			[black, empty, black], 
    			[empty, white, empty]
    		],
    check_win_player(Board4, white),
    check_win_player(Board4, black),
	write('Test 4 passed'), nl, nl,

    % Test 5: Empty board - no win.
    Board5 = [	[empty, empty, empty], 
    			[empty, empty, empty], 
    			[empty, empty, empty]
    		],
    check_win_player(Board5, white),
    check_win_player(Board5, black),
    write('Test 5 passed'), nl, nl,


    % Test 6: No win state for both.
    Board6 = [ [white, empty, black, white, empty],
               [black, empty, white, black, empty],
               [white, empty, black, white, empty],
               [black, empty, white, black, empty] ],
    \+ check_win_player(Board6, white),
    \+ check_win_player(Board6, black),
    write('Test 6 passed'), nl, nl,

    % Test 7: White wins.
    Board7 = [ [empty, empty, white, empty, empty],
               [empty, white, empty, white, empty],
               [white, empty, empty, empty, white],
               [empty, white, empty, black, black] ],
    \+ check_win_player(Board7, white),
    \+ check_win_player(Board7, black),
    write('Test 7 passed'), nl, nl,


    % Test 8: No win state for both because of adjacency.
    Board8 = [ [white, empty, white, empty, black],
               [empty, empty, empty, black, empty],
               [white, empty, black, empty, white],
               [empty, black, empty, white, empty] ],
    \+ check_win_player(Board8, white),
    \+ check_win_player(Board8, black),
    write('Test 8 passed'), nl, nl,

    % Test 9: Black wins.
    Board9 = [ [empty, white, empty, black, empty],
               [black, empty, empty, empty, empty],
               [empty, empty, empty, empty, black],
               [black, white, empty, empty, black] ],
    \+ check_win_player(Board9, black),
    check_win_player(Board9, white),
    write('Test 9 passed'), nl, nl,
    write('All tests passed'), nl.

