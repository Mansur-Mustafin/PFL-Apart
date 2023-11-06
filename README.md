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
- **Continuous jumps:** A piece may execute multiple jumps during a move without landing on the same square twice. Engaging in continuous jumps is optional and can be halted at any point. A single step cannot be included in a sequence of a continuous jump. The first move of the first player cannot be a continuous jump.

### End of the game

Victory is achieved when a player successfully places all their pieces non-adjacent to friendly pieces, horizontally, vertically, and diagonally. If both players reach this state at the same time, the player who made the move loses.

### REFERENCES
For a detailed overview of the rules and gameplay mechanics, visit the following resources:
- [Apart game](https://kanare-abstract.com/en/pages/apart)
- [PDF Version of Rules](https://cdn.shopify.com/s/files/1/0578/3502/8664/files/Apart_EN.pdf?v=1682248406)

## Game Logic

### Internal Game State Representation
- For the representation of the game state we use a matrix `Board` that is a list of lists, filled with pieces: `empty`, `white`, `black`. <br> <br>
- To represent the players we use a structure: `Player-NextPlayer` where `Player` is the current player and `NextPlayer` the second player of game, they could be: `player_black`, `player_white`, `easy_pc_black`, `easy_pc_white`, `hard_pc_white`, `hard_pc_black`, where player_x is a humen player, easy_pc_x is computer player that plays randomly, and hard_pc_x is computer player that plays greedily.<br> <br>
- We store the `Visited` list of positions that were already visited in the case of a human player's turn or a list with the jumps that a computer player will execute on its turn. <br> <br>
- Becouse of the rule `The first move of the first player cannot be a continuous jump`, we also store `FirstMove` that indicate if the move is first of first player. <br> <br>

Possible representations of pieces:
- b - represents the black piece 
- w - represents the white piece
- W - selected piece to make the next jump.
- o - empty position where I can move my selected piece.
- O - enemy piece that I can eat with my selected piece.
- x - position that was already visited in the current turn.

* Initial state:
```
Turn of player playing white              
     A   B   C   D   E
   |---|---|---|---|---|
1  |   | b | b | b |   |                                              
   |---|---|---|---|---|                                              
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

* First move after selecting a piece:
```
Turn of player playing white
     A   B   C   D   E
   |---|---|---|---|---|
1  |   |   | b | b |   |
   |---|---|---|---|---|                                          
2  | b |   |   | O | o |                                          
   |---|---|---|---|---|                                           
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

* Continuous jump:
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
5  | w | o |   | x |   |                                               
   |---|---|---|---|---|
6  |   |   | w | w |   |
   |---|---|---|---|---|
Now, choose your destination on the board.
Enter a valid position in this format Column-Row:   
|:                                                                     
You chose to stop your movement. It's the next player's turn now.      
```

* End of the game
```
     A   B   C   D   E
   |---|---|---|---|---|
1  |   |   | b | w | b |
   |---|---|---|---|---|
2  | b |   | b |   |   |
   |---|---|---|---|---|
3  |   | w |   | w |   |
   |---|---|---|---|---|
4  |   |   |   |   | b |
   |---|---|---|---|---|
5  | w |   | b |   | w |
   |---|---|---|---|---|
6  |   |   | w |   |   |
   |---|---|---|---|---|
[WHITE] The player with white pieces wins!
The game has ended. Do you want to play again? (y/n)
n
Thank you for playing!
true ? 
yes
```

### Game State Visualization:
The files `io.pl` and `view.pl` contain all prints and menus of game. <br> <br>
In our program we have the following menus:
- Main menu with the selection of the game mode;
- Menu to choose the difficulty of a computer.

All Menus are simple lists of options:
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

In our program we interact with the user in the following manners:
- Ask for a position on the board to select a piece or a destination;
- To ask for the size of the board the user desires;
- To ask if the user wants to play again after the game finished;
- Interact of the two menus above.

In our program we handle all possible input errors given by the users by asking for the input again such as the following example:

```
Now, choose your destination on the board.
                                                                                                                 
Enter a valid position in this format Column-Row: 
hello world                                                                                                                                                                                                 
Enter a valid position in this format Column-Row: 
A-6                                                                                                                                                                                                                
Turn of player playing black
     A   B   C   D
   |---|---|---|---|
1  |   | b | b |   |
```

Our predicate that creates the board in the beginning of the program is flexible, since it receives the number of rows and columns the user wants to have in the game up to a certain limit (from 4 to 15 columns and from 6 to 15 rows).
Because of that all of the predicates that display the game board are also flexible. <br> <br>


### Move Validation and Execution

To validate a certain jump we follow the following criteria:
- The position we land on cannot have another piece that is ours;
- The position we land on cannot be a position that has already been visited in the current turn;
- The number of spaces the piece moves must be equal to the length of the line of consecutive pieces, that our piece is part of, in the direction the piece moves;
- If it is the first move of the game we can't have more than one jump;
- We cannot include a step (a jump that moves the piece one space) in a continuous jump.

When the move of a computer is generated it also follows the criteria above.

We execute the move differently for a human player and a computer player:
- **Human player**: we continuously ask for the input of the player for the next jump that he wants to perform. He might decide to end his move at any time, as long as he moved the selected piece at least one;
- **Computer player**: for the computer player we generate its turn previously. Because of that we simply perform each jump separately, displaying the board after each one, until we run out of moves;

To execute a move we simply leave an empty piece in the spot our piece was at and leave our piece at the final position in the player's turn.

### List of Valid Moves
As we have `valid_move(+Player-Board-Visited, ?CurrCol-CurrRow-NewCol-NewRow)` in the case of a human player we simply call the `findall/3` predicate to find all possible moves with the piece in a certain position. In the case of a computer player, we calculate all valid steps in a similar way to how we calculate each level of a `Pascal Triangle`. Firstly we store all the pieces the computer can move. For each one of them we first generate the moves that consist of one jump only. After that, with all the moves with only one jump, we generate all moves with two jumps. We repeat this process of using the moves with N-1 jumps to generate the moves with N jumps until there are no more possible moves. All of this moves are stored in a list that serves as an accumulator to give us the final result with all possible moves. 

### End of Game
We have two main predicates that handle the ending of a game: `game_over(+GameState, -Winner)` and `end_game/3`. The first one determines the winner of the game, if there is one, while the second one outputs the winner and asks if the user wants to play again <br> <br>
The `game_over/2` was implemented using a Window Sliding Technique that checks each pair of adjecent pieces once, to check if any piece of a player is adjacent to another piece of that same player.

### Game State Evaluation
The function `value(+GameState, +Player, -Value)` gives the Value, of the game state where visited list is a of a valid turn for the computer, that is, a series of jumps.<br> <br>
To determine the `Value` we try to maximize the number of separate pieces that we end up with after our turn. We also try to minimize the number of enemy pieces that we eat and the number of new enemy connected components that we form after eating one of its pieces.<br> <br>
The connected components adjacent to the piece that we eat are calculated using a DFS algorithm. <br> <br>

### Computer Plays
Each of the computer player first calculates all of the valid moves he can perform. After that they take different actions:<br>
- At difficulty level 1, the computer selects a move at random, which can be a single step or a continuous jump. <br>
- At difficulty level 2, the computer chooses the move with the highest `Value`; if there are multiple moves with the same highest value, it selects one of those randomly. <br><br>
If it is the first turn of the computer player, he also filters for moves with only a single step as to not break the follwing rule `The first move of the first player cannot be a continuous jump`.


## Conclusions
Overall, we successfully achieved the project's objectives. We experienced the distinction between traditional imperative programming languages and logic programming, and we noted that utilizing predicates in certain cases made problem-solving more intuitive. However, it's important to acknowledge that creating the architecture of the game in traditional languages tends to be more straightforward.

The current implementation, however, has a limitation: the execution speed of the computer player's move is slow on boards larger than 15x15.

For future enhancements, we could consider implementing the game using sockets to enable gameplay on two separate computers. 
Additionally, applying machine learning algorithms to train the computer to play the game would be a fascinating application to observe in Prolog. 
We could also give the decision to cancel a certain move to the player if he misstypes any move.

## Bibliography
[Manual SICStus HTML] (https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/)
[Manual SICStus PDF] (https://sicstus.sics.se/sicstus/docs/latest4/pdf/sicstus.pdf)
