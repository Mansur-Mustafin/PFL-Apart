is_empty(empty).

process_move([Col-Row], Board, Player, -1, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	is_empty(Value),

	my_piece(Player, MyPiece),
	set_value_at(Board, Row, Col, MyPiece, NewBoard).

process_move([Col-Row], Board, Player, 0, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	\+ is_empty(Value),

	my_piece(Player, MyPiece),
	set_value_at(Board, Row, Col, MyPiece, NewBoard).
	
% se eu botei peca no espaco com peca black.
process_move([Col-Row|RestMoves], Board, Player, NumberEatPieces, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	\+ is_empty(Value),

	process_move(RestMoves, Board, Player, N1, TmpBoard),
	NumberEatPieces is N1 + 1,
	set_value_at(TmpBoard, Row, Col, empty, NewBoard).

% se eu botei peca no espac vazio.
process_move([Col-Row|RestMoves], Board, Player, NumberEatPieces, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	is_empty(Value),

	process_move(RestMoves, Board, Player, NumberEatPieces, NewBoard).
	
	
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


test:-
	get_board1(Board),
	display_board(Board), nl, nl,
	
	get_number_of_separate_pieces(Board, player_white, Nbefore),
	process_move([1-5, 2-5, 2-1], Board, player_white, N, NB),
	get_number_of_separate_pieces(Board, player_white, Nafter),
	
	display_board(NB),
	
	write('Numbero de pecas eu comi: '), write(N), nl,
	write('Numbero de pecas separadas antes: '), write(Nbefore), nl,
	write('Numbero de pecas separadas depois: '), write(Nafter), nl.
	

