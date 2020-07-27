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
    + Position
    + Number of moves
    + Next possible spaces
	- Expressed as update function??
- Chess board

