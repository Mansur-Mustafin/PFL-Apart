% TODO: issue with 2 Enters.
% read_number(X).
read_namber(X):-
	read_namber_aux(0, false, X).

read_namber_aux(Acc, _, X):-
	get_code(C),
	C >= 48,						% '0'
	C =< 57,						% '9'
	!,
	Acc1 is 10 * Acc + (C - 48),
	read_namber_aux(Acc1, true, X).

read_namber_aux(X, true, X):-
	clear_buffer.


% clear buffer/0
clear_buffer:-
	repeat,
	get_char(C),
	C = '\n',
	!.