process_turn([Col-Row, none-none], Board, Player, -1, NewBoard):-
	get_value_at(Board, Row, Col, Value), 

	is_empty(Value),

	my_piece(Player, MyPiece), 

	set_value_at(Board, Row, Col, MyPiece, NewBoard).

process_turn([Col-Row, none-none], Board, Player, 0, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	\+ is_empty(Value), 

	my_piece(Player, MyPiece),
	set_value_at(Board, Row, Col, MyPiece, NewBoard).

% se eu botei peca no espaco com peca black.
process_turn([Col-Row|RestMoves], Board, Player, NumberEatPieces, NewBoard):-
	get_value_at(Board, Row, Col, Value), 
	\+ is_empty(Value), 

	process_turn(RestMoves, Board, Player, N1, TmpBoard), 
	NumberEatPieces is N1 + 1, 

	set_value_at(TmpBoard, Row, Col, empty, NewBoard).

% se eu botei peca no espac vazio.
process_turn([Col-Row|RestMoves], Board, Player, NumberEatPieces, NewBoard):-

	get_value_at(Board, Row, Col, Value),

	is_empty(Value),

	process_turn(RestMoves, Board, Player, NumberEatPieces, NewBoard).


% ================================================================================


get_adj(Col-Row, AdjCol-AdjRow):-
	explore(Col-Row, AdjCol-AdjRow, _, _).


adj_pieces(Board, Col, Row, Player):-
	my_piece(Player, PlayersPiece),

	setof(AdjCol-AdjRow, (Value, Board)^(
		get_adj(Col-Row, AdjCol-AdjRow),
		in_bounds(Board, AdjCol-AdjRow),
		get_value_at(Board, AdjRow, AdjCol, Value),
		same_piece(PlayersPiece, Value, _)
	), _).


get_number_of_separate_pieces(Board, Player, N):-
	shape(Board, Rows, Columns),
    Rows1 is Rows - 1,
    Columns1 is Columns - 1,

    findall(_, (
        between(0, Rows1, Row),
        between(0, Columns1, Col),
        get_value_at(Board, Row, Col, Value),
        my_piece(Player, Value),
        \+ adj_pieces(Board, Col, Row, Player)
        ), ValidMoves),
    length(ValidMoves, N).

% value(+GameState, +Player, -Value)
value(Player-_-Board-Turn, Player, Value) :-
	get_number_of_separate_pieces(Board, Player, Nbefore),
	process_turn(Turn, Board, Player, NumberEatPieces, NewBoard), !,
	get_number_of_separate_pieces(NewBoard, Player, Nafter),

	Value is 2 * (Nafter - Nbefore) - NumberEatPieces.

test:-
	Board = [[empty,black,black,empty],[empty,black,black,empty],[empty,empty,white,empty],[empty,empty,empty,empty],[empty,white,empty,empty],[empty,white,white,empty]],
	% display_board(Board), nl, nl,

	get_number_of_separate_pieces(Board, hard_pc_black, Nbefore),
	process_turn([2-1,1-2,none-none], Board, hard_pc_black, N, NB), !,
	get_number_of_separate_pieces(NB, hard_pc_black, Nafter),

	% display_board(NB),

	write('Numbero de pecas eu comi: '), write(N), nl,
	write('Numbero de pecas separadas antes: '), write(Nbefore), nl,
	write('Numbero de pecas separadas depois: '), write(Nafter), nl.