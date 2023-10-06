% game_state/2
% game_state(+Player, +Board)
game_state(player_white,
	[
	[empty, black, black, black, black, black, black, empty],
	[empty, black, black, black, black, black, black, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, white, white, white, white, white, white, empty],
	[empty, white, white, white, white, white, white, empty],
	]).


%player/2
%player(+CurrentPlayer, -NextPlayer)
switch_player(player_white, player_black).
switch_player(player_black, player_white).

