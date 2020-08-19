module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A helpful kind synonym!
type Type = *

data Vec (n :: Nat) (a :: Type) where
    VEnd   :: Vec 0 a
    (:->)  :: a -> Vec n a -> Vec (n + 1) a
infixr 4 :->

-- Helper type family, to avoid the (:-> VEnd) bit.
type family (:<>) (x :: a) (y :: a) :: Vec 2 a where
    x :<> y = x :-> (y :-> VEnd)
infixr 5 :<>

-- Type synonym for an 8x8 grid
type Grid8x8 = Vec 8 (Vec 8 (Maybe Piece))

-- TODO: Dimensions of board in type??
data Board where
    MkBoard :: Grid8x8 -> Board

data Piece where
    MkPiece :: Team -> PieceName -> PieceInfo -> Piece

data Team = Black | White

-- Make singleton types for each piece??
-- data Pawn = Pawn
-- data Bishop = Bishop
-- data Knight = Knight
-- data Rook = Rook
-- data King = King
-- data Queen = Queen
data PieceName = Pawn
               | Bishop
               | Knight
               | Rook
               | King
               | Queen

-- Holds the number of moves they've made, plus their current position.
-- While their position is implicit from where they are in the board, it's
-- helpful!
data PieceInfo where
    Info :: Nat -> Position -> PieceInfo

-- TODO: Type level char??
-- TODO: Make operator for adding a Nat to a row name, creating a new row name (e.g. "a" :+: 1)
data Position where
    At :: Symbol -> Nat -> Position

type family ValidRow (row :: Symbol) :: Symbol where
    ValidRow "a" = "a"
    ValidRow "b" = "b"
    ValidRow "c" = "c"
    ValidRow "d" = "d"
    ValidRow "e" = "e"
    ValidRow "f" = "f"
    ValidRow "g" = "g"
    ValidRow "h" = "h"
    ValidRow x = TypeError (Text "This row is not a valid symbol!")

-- Type families to add an offset to rows!
-- e.g. Pawn moves from row to row :+ 1, or row :- 1
-- TODO: Parameterise these with the number of rows somehow??
type family (:+) (row :: Symbol) (offset :: Nat) :: Symbol where
    row :+ 0 = ValidRow row
    "a" :+ 1 = "b"
    "b" :+ 1 = "c"
    "c" :+ 1 = "d"
    "d" :+ 1 = "e"
    "e" :+ 1 = "f"
    "f" :+ 1 = "g"
    "g" :+ 1 = "h"
    "h" :+ 1 = ValidRow "z"
    row :+ n = (row :+ 1) :+ (n - 1)
type family (:-) (row :: Symbol) (offset :: Nat) :: Symbol where
    row :- 0 = ValidRow row
    "a" :- 1 = ValidRow "z"
    "b" :- 1 = "a"
    "c" :- 1 = "b"
    "d" :- 1 = "c"
    "e" :- 1 = "d"
    "f" :- 1 = "e"
    "g" :- 1 = "f"
    "h" :- 1 = "g"
    row :- n = (row :- 1) :- (n - 1)


data Proxy a = Proxy

-- TEST TYPES
-- TODO: Remove these
type TestPosition = At "a" 1
type TestPiece    = MkPiece Black Pawn (Info 0 TestPosition)
type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type TestBoard    = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow
type TestBoard2   = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow

type family IsUpdateValid (from :: Board) (to :: Board) (turn :: Team) :: Board where
    IsUpdateValid x x _ = TypeError (Text "A move must be made - the board cannot stay exactly the same.")
    IsUpdateValid _ _ _ = TypeError (Text "Unfinished!")

-- Rudimentary way to display type errors, for now.
x :: Proxy (IsUpdateValid (MkBoard TestBoard) (MkBoard TestBoard2) White)
x = Proxy






-----------------------------------------------------------------------------------------------

-- type family Update (board :: Board) (pieces :: Vec n Piece) (positions :: Vec n Position) :: Board where
--     Update board pieces positions = TypeError (Text "Unfinished!")

-- -- TODO: Make this work
-- -- Associated type family for pieces that can move; takes their current position, the board,
-- -- and then a series of valid next boards!
-- -- TODO: Use type equality (~) to check that the piece is at that position
-- class Moveable p where
--     type NextPositions p :: Position -> Board -> Exp (Vec n Position)

-- -- Defunctionalised evaluation of moveable pieces
-- type Exp a = a -> Type

-- type family Eval (e :: Exp a) :: a

-- data MovePawn :: Position -> Board -> Exp (Vec n Position)

-- -- TODO: Complete the below by checking the piece at the location!
-- -- FIXME: Pawns move in different directions depending on their color - fix this
-- -- FIXME: Pawns move 1 or 2 spaces depending on whether it's their first move or not
-- type instance Eval (MovePawn (At row col) b) = (At row (col + 1)) :-> ((At row (col + 2)) :-> VEnd)

-- instance Moveable Pawn where
--     type NextPositions Pawn = MovePawn

-- type family MovePawn (pos :: Position) (b :: Board) :: Vec n Position where
--     MovePawn x y = TestPosition :-> VEnd
