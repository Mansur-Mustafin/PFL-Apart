:- ensure_loaded('main.pl').

/*
    File: io.pl
    Description: 
    This file contains all predicates that interact with the user by
    prompting him to input information or printing the game's information.
*/


/*
	read_pos(-Move, +HasMove)
	Description: read_pos/2 prompts the user to input a valid move in the form Column-Row
	if the the player playing has a possible move determined by HasMove. If not, the move 
	none-none will unify with Move.
*/
% Current player doesn't have a valid move
read_pos(none-none, false).
read_pos(Col-Row, true) :-
	write('Enter a valid position in this format Column-Row: '), nl,
	read_collumn(Col),
	read_dash,
	read_row(Row),
	!.

read_pos(none-none, true) :-
	peek_char(Char),
	Char = '\n',
	get_char(_), !.

% An error has occured, try to read a move from the beginning
read_pos(Col-Row, true) :-
	clear_buffer,
	read_pos(Col-Row, true).

/*
	read_collumn(-Col)
	Description: read_collumn/1 reads a character input by the user and validates it. The character must
	be an uppercase or lowercase letter from the alphabet (a-z or A-Z). Col will unify with
	the 0-indexed position of the uppercase letter in the alphabet, for examples, A corresponds to 0,
	B to 1, C to 2...
*/
read_collumn(Col) :-
	peek_code(Char),
	validate_letter(Char, Letter),
	Col is Letter - 65, 
	get_code(_), !.

% Case where the character is an uppercase letter from the alphabet
validate_letter(Letter, Letter) :-
	Letter >= 65,
	Letter =< 90.

% Case where the character is a lowercase letter from the alphabet
validate_letter(Char, Letter) :-
	Char >= 97,
	Char =< 122,
	Letter is Char - 32.

/*
	Description: read_dash/0 determines if the user inputs the character '-'
*/
read_dash :-
	peek_char(Char),
	Char = '-',
	get_char(_).

/*
	read_row(-Row)
	Description: read_row/1 reads a number input by the user. Row will unify with the number read
	subtracted by one to represent a 0-indexed value.
*/
read_row(Row) :-
	read_row_aux(0, false, X),
	Row is X - 1.

/*
	read_row_aux(+Acc, +ReadOneDigit, -Number)
	Description: read_row_aux/3 deals with reading a number from the input of a user. ReadOneDigit
	determines if at least a digit has already been read. Number will unify with the valid number
	the user gave as input.
	Inputs like 'a123' and '123a' will not be accepted although it has a number in it 
*/
read_row_aux(Acc, _, X):-
	peek_code(C),
	C >= 48,
	C =< 57,
	get_code(_),
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_row_aux(Acc1, true, X).

% Checks if the last character is \n and cleans the buffer
read_row_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer.

/*
	read_number(-Number, +Low, +Up)
	Description: read_number/3 reads a number input by the user. Number will unify with the number read
	if the Number is between the values Low and Up, inclusive. The user will be asked for input until
	he inputs a valid number
*/
read_number(X, Low, Up):-
	repeat,
	read_number_aux(0, false, X),
	between(Low, Up, X),
	!.

/*
	read_number_aux(+Acc, +ReadOneDigit, -Number)
	Description: read_row_aux/3 deals with reading a number from the input of a user. ReadOneDigit
	determines if at least a digit has already been read. Number will unify with the valid number
	the user gave as input.
	Inputs like 'a123' and '123a' will not be accepted although it has a number in it 
*/
read_number_aux(Acc, _, X):-
	peek_code(C),
	C >= 48,							% '0'
	C =< 57,
	get_code(_),						% '9'
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_number_aux(Acc1, true, X).

% Fails if user only presses 'Enter'
read_number_aux(0, false, _):-
	write('Please enter number'), nl,
	clear_buffer, fail.


read_number_aux(X, true, X):-
	peek_char(C),
	C = '\n',
	clear_buffer, !.

% Fails if the last character read isn't \n, since the user wrote a non-digit character after the number
read_number_aux(X, true, X):-
	peek_char(C),
	C \= '\n',
	write('Please enter number'), nl,
	clear_buffer,
	fail.


/*
	Description: clear_buffer/0 reads all pending input in the input buffer, leaving it empty
*/
clear_buffer:-
	repeat,
	get_char(C),
	C = '\n',
	!.


/*
	get_mode(-Mode)
	Description: get_mode/1 unifies Mode with the mode the user wants to play, from its input
*/
get_mode(Mode) :-
	print_main_menu,
	read_number(Mode, 1, 4).


/*
	get_board_size(-NumCol-NumRow)
	Description: get_board_size/1 unifies NumCol and NumRow with the number of columns and rows
	the user writes as input
*/
get_board_size(NumCol-NumRow) :-
	write('Please enter a size of board columns between 4 and 15: '), nl,
	read_number(NumCol, 4, 15),
	write('Please enter a size of board rows between 6 and 15: '), nl,
	read_number(NumRow, 6, 15).


/*
	create_players(-FirstPlayer-SecondPlayer)
	Description: create_players/1 unifies FirstPlayer and SecondPlayer with players
	according to chosen mode.
*/
create_players(FirstPlayer-SecondPlayer):-
    get_mode(Lvl),
    createPlayer(Lvl, TempFirstPlayer-TempSecondPlayer),
    choose_computer(TempFirstPlayer, white, FirstPlayer),
    choose_computer(TempSecondPlayer, black, SecondPlayer).