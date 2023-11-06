:- ensure_loaded('main.pl').

/*
    File: computer.pl
    Description: 
    This file contains all predicates that decide how the computer players move
*/


/*
	get_number_of_separate_pieces(+Board, +Player, -N)
	Description: get_number_of_separate_pieces/3 unifies N with the number of pieces from
	the Player that don't have any other piece of Player adjacent to them vertically, horizontally or diagonally
*/
get_number_of_separate_pieces(Board, Player, N):-
    findall(_, (
        get_value_at(Board, Row, Col, Value),
        my_piece(Player, Value),
        adj_pieces(Board, Col, Row, Value, List),
        length(List, 0)
        ), ValidMoves),
    length(ValidMoves, N).


/*
	adj_pieces(+Board, +Col, +Row, +Piece, -List)
	Description: adj_pieces/5 unifies List with a list of all positions on the board have the piece Piece
	and are adjecent to the position Col-Row
*/
adj_pieces(Board, Col, Row, Piece, List):-
    findall(AdjCol-AdjRow, (
        get_adj(Col-Row, AdjCol-AdjRow),
        get_value_at(Board, AdjRow, AdjCol, Value),
        same_piece(Piece, Value, _)
    ), List).


/*
	get_adj(+Col-Row, -AdjCol-AdjRow)
	Description: get_adj/2 unifies AdjCol-AdjRow with an adjacent position to Col-Row
*/
get_adj(Col-Row, AdjCol-AdjRow):-
    explore(Col-Row, AdjCol-AdjRow, _, _).


/*
	process_turn(+Turn, +Board, +Player, -InfluenceRate, -NewBoard)
	Description: process_turn/5 simulates the movement of a piece according to the sequence
	of jumps in the list Turn, keeping track of the number of enemy pieces that are eaten and
	the number of different groups of enemy pieces that are separated in the process. NewBoard
	will unify with the Board after moving the piece through the Turn.
*/

% Case where the player doesn't eat an enemy piece in its last move
process_turn([Col-Row, none-none], Board, Player, 0, NewBoard):-
	get_value_at(Board, Row, Col, Value), 

	is_empty(Value),

	my_piece(Player, MyPiece), 

	set_value_at(Board, Row, Col, MyPiece, NewBoard).

% Case where the player eats an enemy piece in its last move
process_turn([Col-Row, none-none], Board, Player, InfluenceRate, NewBoard):-
	get_value_at(Board, Row, Col, Value),
	\+ is_empty(Value), 

	my_piece(Player, MyPiece),
	set_value_at(Board, Row, Col, MyPiece, NewBoard),
	check_components(Col-Row, Value, NewBoard, [Col-Row], 0, Components),
	InfluenceRate is 0.4 + Components.

% Case where the player eats an enemy piece but not in its last move
process_turn([Col-Row|RestMoves], Board, Player, InfluenceRate, NewBoard):-
	get_value_at(Board, Row, Col, Value), 
	\+ is_empty(Value), 

	process_turn(RestMoves, Board, Player, N1, TmpBoard), 
	check_components(Col-Row, Value, TmpBoard, [Col-Row], 0, Components),
	InfluenceRate is N1 + 0.4 + Components,

	set_value_at(TmpBoard, Row, Col, empty, NewBoard).

% Case where the player doesn't eat an enemy piece but not in its last move
process_turn([Col-Row|RestMoves], Board, Player, InfluenceRate, NewBoard):-

	get_value_at(Board, Row, Col, Value),

	is_empty(Value),

	process_turn(RestMoves, Board, Player, InfluenceRate, NewBoard).


/*
	check_components(+Pos, +Piece, +Board, +Visited, +Acc, -N)
	Description: check_components/5 unifies N with the number of connected components
	formed by pieces of type Piece that are adjacent to Pos, after the removal of the piece in Pos.
*/
check_components(Col-Row, Piece, Board, Visited, Acc, N) :-
	adj_pieces(Board, Col, Row, Piece, Adjs),
	member(NewCol-NewRow, Adjs),
	\+ member(NewCol-NewRow, Visited), !,
	Acc1 is Acc + 1,
	dfs(NewCol-NewRow, Piece, Board, [NewCol-NewRow | Visited], NewVisited), !,
	check_components(Col-Row, Piece, Board, NewVisited, Acc1, N).

% Case where we remove an isolated piece
check_components(_, _, _, _, 0, 0) :- !.

check_components(_, _, _, _, N, N1) :- N1 is N - 0.5.

/*
	dfs(+Pos, +Piece, +Board, +Visited, -NewVisited)
	Description: dfs/5 performs a depth-first search throught the adjacent pieces of the same type
	as Piece from position Pos, unifying NewVisited with the pieces that get visited in the search
*/
dfs(Col-Row, Piece, Board, Visited, NewVisited) :-
	adj_pieces(Board, Col, Row, Piece, Adjs),
	member(NewCol-NewRow, Adjs),
	\+ member(NewCol-NewRow, Visited),
	dfs(NewCol-NewRow, Piece, Board, [NewCol-NewRow | Visited], NextVisited),
	check_over(Col-Row, Piece, Board, NextVisited, NewVisited).

% If when trying to visit the adjacent pieces in a certain position we can't get to a new position we end the dfs
dfs(_, _, _, Visited, Visited).


/*
	check_over(+Pos, +Piece, +Board, +NextVisited, -NewVisited)
	Description: check_over/5 calls a new depth-first search in the same position as we were before
	to guarantee we search all adjacent pieces, with the updated list of visited positions
*/
check_over(Col-Row, Piece, Board, NextVisited, NewVisited) :-
	dfs(Col-Row, Piece, Board, NextVisited, NewVisited).
