:- consult('view.pl').
:- consult('state.pl').

play :-
	createBoard(Board),
	game_state(Player, Board, Visited),
	display_game([Player, Board, Visited]).
