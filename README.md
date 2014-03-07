9 Man Morris Game
====================

Requirements
---------------------
Haskell platform

Modelling the game
---------------------

The board looks like the following

>0-----------------1-----------------2  
| . . . . . . . . . . .  | . . . . . . . . . .  .  | <br>
| . . 8------------9-----------10 . .  | <br>
| . . . |  . . . . . . .  | . . . . . . . | . . .  |<br>
| . .  . |   . .  16----17----18 . .| . . . | <br>
| . . . |  . . . | . .  . . . . . .  |. . . | . . .  |<br>
7. . 15   . . 23 . . . . . .19 . .11 . . 3 <br>
| . . . |  . . . | . .  . . . . . .  |. . . | . . .  |<br>
| . .  . |   . .  22----21----20 . .| . . . | <br>
| . . . |  . . . . . . .  | . . . . . . . | . . . |<br>
| . . 14----------13----------12 . .  | <br>
| . . . . . . . . . . .  | . . . . . . . . . .  .  | <br>
6-----------------5-----------------4  


The board is modeled as a 3x8 matrix where:
<ul>
<li> the first line is the big square: 0,2,3,...7 </li>
<li> the second line is the second square: 8,9,10,..15</li>
<li> the third line is the inner square: 16, 17, ...23 </li>
</ul>
>The game has 2 phases. First is placing the tokens; each player has to place 9 tokens.
In the second phase, the players move their tokens in order to form mills (3 tokens in a line) and to capture the opponent's tokens.

The matrix model of the board provides simple rules for the movement:
<ul>
<li> on the same line is possible to move on adiacent positions </li>
<li> is possible to move form one line to the other in the matrix only if the position is odd </li>
</ul>

