# machikoro-engine

## Code Design
The parser returns `Either String Board`.  There are two kinds of commands - update commands, that change the board, or show commands that display the state of the board.  Show commands are returned immediately by `parseCommand`, and update commands are passed into the more specific methods.

There is a file for the Board, that includes card movement, and a file for the Player, that just builds an initial player.

## Current problem

How to save turn history
