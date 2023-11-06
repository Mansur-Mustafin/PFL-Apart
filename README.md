# Apart

## Group : Apart_1
* Members:
  * up202108771 - Daniel dos Santos Ferreira - 50%
  * up202102355 - Mansur Mustafin - 50%

## Installation and Execution:
If you have not installed SICStus on yor pc use [manual](http://sicstus.sics.se/sicstus/products4/). <br>
Download the zip of code from GitHub, and extract to some folder:
* Linux, open the exctracted folder in terminal and run commands:
  ```
  cd src/
  sicstus
  consult('main.pl').
  play.
  ```
* Windows, open the `SICStus` and using menu, consult the `mai.pl` file, the run `play.` in terminal of SICStus.

## Description of the game

Apart is a board game in which the goal is to distribute all of your pieces.

### Setup

- **Board Configuration:** Utilize an 8x8 grid for a game.
- **Piece Placement:** Each player controls 12 pieces. White pieces occupy two rows of 6 at one edge of the board, flanked by empty spaces. Symmetrically opposite, the black pieces mirror this setup.

### Key Definitions

- **Line:** An unbroken, straight alignment of same-color pieces, oriented vertically, horizontally, or diagonally.
- **Length of a Line:** The number of pieces constituting the line.
- **Single Step:** A move to an immediately adjacent square, regardless of direction.
- **Jump:** A move spanning two or more squares.

### Gameplay Dynamics

Players alternate turns, initiating with the player controlling the white pieces. Each player has the opportunity to move one of his pieces one or more times according to the movement rules below. Note that the first move of the game cannot be a continuous jump.

### Movement rules

- **Direction and Distance:** Pieces shift along one of their respective lines, traveling a distance corresponding to the line's length.
- **Jump Mechanics:** Pieces can leap over others, capturing any opponent's piece upon landing. Players cannot move their pieces to squares already occupied by another one of their own pieces.
- **Continuous jumps:** A piece may execute multiple jumps during a move without landing on the same square twice. Engaging in continuous jumps is optional and can be halted at any point. A single step cannot be included in a sequence of a continuous jump.

### End of the game

Victory is achieved when a player successfully places all their pieces non-adjacent to friendly pieces, horizontally, vertically, and diagonally. If both players reach this state at the same time, the player who made the move loses.

### REFERENCES
For a detailed overview of the rules and gameplay mechanics, visit the following resources:
- [Apart game](https://kanare-abstract.com/en/pages/apart)
- [PDF Version of Rules](https://cdn.shopify.com/s/files/1/0578/3502/8664/files/Apart_EN.pdf?v=1682248406)

## Game Logic

### Internal Game State Representation
For respresentation of the game state we use matrix `Board` that is list of lists, filled by pieces: `empty`, `white`, `black`. <br> <br>
For represent the players we use structure: `Player-NextPlayer` where `Player` is current player and `NextPlayer` the second player of game, they could be: `player_black`, `player_white`, `easy_pc_black`, `easy_pc_white`, `hard_pc_white`, `hard_pc_black`, Where player_x is a humen player, easy_pc_x is computer player lvl 1, and hard_pc_x is computer player lvl 2.<br> <br>
We store the `Visited` list of pieces that was visited of each round, where round is continous jump of current player. <br> <br>
Becouse of the rules of the Apart, we store `FirstMove` that indicate if the move is first of first player. <br> <br>

* Initial state:
```
Turn of player playing white              
     A   B   C   D   E
   |---|---|---|---|---|
1  |   | b | b | b |   |                                              % b - represent the black piece 
   |---|---|---|---|---|                                              % w - represent the white piece
2  |   | b | b | b |   |
   |---|---|---|---|---|
3  |   |   |   |   |   |
   |---|---|---|---|---|
4  |   |   |   |   |   |
   |---|---|---|---|---|
5  |   | w | w | w |   |
   |---|---|---|---|---|
6  |   | w | w | w |   |
   |---|---|---|---|---|
Please select the piece you wish to move.
Enter a valid position in this format Column-Row: 
C-5
```
* First move after selection piece:
```
Turn of player playing white
     A   B   C   D   E
   |---|---|---|---|---|
1  |   |   | b | b |   |
   |---|---|---|---|---|                                          % W - selected piece to make next jump.
2  | b |   |   | O | o |                                          % o - is empty place where I can move selected piece.
   |---|---|---|---|---|                                          % O - is enemy piece what I can eat. 
3  |   | w |   | o | W |
   |---|---|---|---|---|
4  |   | b |   | o | O |
   |---|---|---|---|---|
5  | w |   |   | w |   |
   |---|---|---|---|---|
6  |   |   | w | w |   |
   |---|---|---|---|---|
Now, choose your destination on the board.
Enter a valid position in this format Column-Row: 
E-2
```
* Continues jump:
```
Turn of player playing white
     A   B   C   D   E
   |---|---|---|---|---|
1  |   |   | b |   |   |
   |---|---|---|---|---|
2  | b |   | b | b | w |
   |---|---|---|---|---|
3  |   | w |   | W |   |
   |---|---|---|---|---|
4  |   | b |   |   | b |
   |---|---|---|---|---|
5  | w | o |   | x |   |                                               % x - this place was visited.
   |---|---|---|---|---|
6  |   |   | w | w |   |
   |---|---|---|---|---|
Now, choose your destination on the board.
Enter a valid position in this format Column-Row:   
|:                                                                     % we stop our round
You chose to stop your movement. It's the next player's turn now.      % and now is the round of second player.
```

### Game State Visualization:
The files `io.pl` and `view.pl` contain all prints and menus of game. <br> <br>
All Menus is simple list of options:
```
Choose mode:
Player vs Player........[1]
Player vs Computer......[2]
Computer vs Player......[3]
Computer vs Computer....[4]
2
Choose the difficulty of the black computer
Easy.....[1]
Hard.....[2]
2
```
Where the program read the user input, with possibles error hendlers. TODO! <br> <br>
Functions of dysplay game board in general are recursive and simple. <br> <br>
Functions of creation of game state is componed of different funcions one of them is `createBoard(NumCol-NumRow, Board)` that create retanguar `Board`. <br> <br>


### Move Validation and Execution


### List of Valid Moves


### End of Game
This case handle 2 main funcions `game_over(+GameState, -Winner)` and `end_game/3` first determine the winner and second finish the game. <br> <br>
The `game_over/2` implemented using Window Sliding Technique that check onec each pair of adjecent pieces.

### Game State Evaluation
The function `value(+GameState, +Player, -Value)` give the Value, of game state where visited list is possible move.<br> <br>
To determine the `Value` we try to maximize the number our separate pieces. And minimize the number eat enemy pieces and number of creation connected compounents.<br> <br>
The connected compounents is calculating using DFS algorithm. <br> <br>

### Computer Plays
The computer player calculates all valid moves.<br>
At difficulty level 1, the computer selects a move at random, which can be a single step or a continuous jump. <br>
At difficulty level 2, the computer chooses the move with the highest `Value`; if there are multiple moves with the same highest value, it selects one at random. <br><br>












## Conclusions
## Bibliography
