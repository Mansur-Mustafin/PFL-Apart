:- ensure_loaded('main.pl').

get_number_of_separate_pieces(Board, Player, N):-
    findall(_, (
        get_value_at(Board, Row, Col, Value),
        my_piece(Player, Value),
        adj_pieces(Board, Col, Row, Value, List),
        length(List, 0)
        ), ValidMoves),
    length(ValidMoves, N).


adj_pieces(Board, Col, Row, Piece, List):-
    findall(AdjCol-AdjRow, (
        get_adj(Col-Row, AdjCol-AdjRow),
        get_value_at(Board, AdjRow, AdjCol, Value),
        same_piece(Piece, Value, _)
    ), List).


get_adj(Col-Row, AdjCol-AdjRow):-
    explore(Col-Row, AdjCol-AdjRow, _, _).


process_turn([Col-Row, none-none], Board, Player, 0, NewBoard):-
	get_value_at(Board, Row, Col, Value), 

	is_empty(Value),

	my_piece(Player, MyPiece), 

	set_value_at(Board, Row, Col, MyPiece, NewBoard).

process_turn([Col-Row, none-none], Board, Player, InfluenceRate, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	\+ is_empty(Value), 

	my_piece(Player, MyPiece),
	set_value_at(Board, Row, Col, MyPiece, NewBoard),
	check_components(Col-Row, Value, NewBoard, [Col-Row], 0, Components),
	InfluenceRate is 0.4 + Components.

% se eu botei peca no espaco com peca black.
process_turn([Col-Row|RestMoves], Board, Player, InfluenceRate, NewBoard):-
	get_value_at(Board, Row, Col, Value), 
	\+ is_empty(Value), 

	process_turn(RestMoves, Board, Player, N1, TmpBoard), 
	check_components(Col-Row, Value, TmpBoard, [Col-Row], 0, Components),
	InfluenceRate is N1 + 0.4 + Components,

	set_value_at(TmpBoard, Row, Col, empty, NewBoard).

% se eu botei peca no espac vazio.
process_turn([Col-Row|RestMoves], Board, Player, InfluenceRate, NewBoard):-

	get_value_at(Board, Row, Col, Value),

	is_empty(Value),

	process_turn(RestMoves, Board, Player, InfluenceRate, NewBoard).


% ================================================================================


check_components(Col-Row, Piece, Board, Visited, Acc, N) :-
	adj_pieces(Board, Col, Row, Piece, Adjs),
	member(NewCol-NewRow, Adjs),
	\+ member(NewCol-NewRow, Visited), !,
	Acc1 is Acc + 1,
	dfs(NewCol-NewRow, Piece, Board, [NewCol-NewRow | Visited], NewVisited), !,
	check_components(Col-Row, Piece, Board, NewVisited, Acc1, N).

check_components(_, _, _, _, 0, 0) :- !.
check_components(_, _, _, _, N, N1) :- N1 is N - 0.5.

dfs(Col-Row, Piece, Board, Visited, NewVisited) :-
	adj_pieces(Board, Col, Row, Piece, Adjs),
	member(NewCol-NewRow, Adjs),
	\+ member(NewCol-NewRow, Visited),
	dfs(NewCol-NewRow, Piece, Board, [NewCol-NewRow | Visited], NextVisited),
	check_over(Col-Row, Piece, Board, [NewCol-NewRow | Visited], NextVisited, NewVisited).

dfs(_, _, _, Visited, Visited).

check_over(Col-Row, Piece, Board, _, NextVisited, NewVisited) :-
	dfs(Col-Row, Piece, Board, NextVisited, NewVisited).
