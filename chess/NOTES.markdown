# Overall plan

Make some way of playing chess with two players, checking its' validity at the type level.

1. Think of a representation

e.g.

```haskell
do
    pawn b2 c2
    knight a1 b2
    -- etc etc
```

2. Think of how to transform a text file into that representation??

3. Specify what exists on the term level, type level, and kind level

## Term level

- Positions (e.g. a4, g8, ...)
- Piece names

## Type level

- Positions
- Piece representations, which needs to include:
  - Position
  - Number of moves
  - Next possible spaces
  - Expressed as update function??
- Chess board

# 27th August 2020

Maybe it would be wise to display the game differently.

Imagine:

- A group of teams
- A series of board states
  - Instead of each piece showing the places it can move, it could show the next board state after it moves!
- A rule checker, ensuring that each board transition is valid
- Some event handler, ensuring that side effects of moves are considered! e.g.
  - Replace a piece with another
  - End the game
  - Move a piece that isn't moved (castling??)

So the reusable/abstracted/proxied parts need to be:

- Team
- Grid/board
  - Chess, it can be 8x8, but noughts and crosses is 3x3 - it needs to be flexible
- Rule checker
  - Maybe allow the rule checker to specify the available teams, as well as the dimensions of the grid/board??
- Event handler??
