module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A helpful kind synonym!
type Type = *
-- A datatype for Proxy types!
data Proxy a = Proxy

data Vec (n :: Nat) (a :: Type) where
    VEnd   :: Vec 0 a
    (:->)  :: a -> Vec (n - 1) a -> Vec n a
infixr 4 :->

-- Helper type family, to avoid the (:-> VEnd) bit.
type family (:<>) (x :: a) (y :: a) :: Vec 2 a where
    x :<> y = x :-> y :-> VEnd
infixr 5 :<>

type family VecToList (v :: Vec n a) :: [a] where
    VecToList VEnd         = '[]
    VecToList (x :-> rest) = x ': (VecToList rest)

type family In (x :: a) (ys :: Vec n a) :: Bool where
    In x (x :-> rest) = 'True
    In x (y :-> rest) = In x rest
    In x VEnd         = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True  _ = 'True
    Or 'False y = y

-- type family If (cond :: Bool) (then :: a) (else :: a) :: a where
--     If 'True then _  = then
--     If 'False _ else = else

-- Type synonym for an 8x8 grid
type Grid8x8 = Vec 8 (Vec 8 (Maybe Piece))

-- TODO: Dimensions of board in type??
type Board = Grid8x8
-- data Board where
--     MkBoard :: Grid8x8 -> Board

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
-- Goes column-row, e.g. At "a" 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Symbol -> Nat -> Position

type Move = (Piece, Position)

-- TODO: Constrain so n < 3??
-- Can get the piece from the initial Position pair
data Moves where
    Moves :: Vec n Move -> Moves

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

-- Type families to add an offset to columns!
-- e.g. Pawn moves from column to column :+ 1, or column :- 1
-- TODO: Parameterise these with the number of rows somehow??
type family (:+) (col :: Symbol) (offset :: Nat) :: Symbol where
    col :+ 0 = ValidRow col
    "a" :+ 1 = "b"
    "b" :+ 1 = "c"
    "c" :+ 1 = "d"
    "d" :+ 1 = "e"
    "e" :+ 1 = "f"
    "f" :+ 1 = "g"
    "g" :+ 1 = "h"
    "h" :+ 1 = ValidRow "z"
    col :+ n = (col :+ 1) :+ (n - 1)
type family (:-) (col :: Symbol) (offset :: Nat) :: Symbol where
    col :- 0 = ValidRow col
    "a" :- 1 = ValidRow "z"
    "b" :- 1 = "a"
    "c" :- 1 = "b"
    "d" :- 1 = "c"
    "e" :- 1 = "d"
    "f" :- 1 = "e"
    "g" :- 1 = "f"
    "h" :- 1 = "g"
    col :- n = (col :- 1) :- (n - 1)

-- TEST TYPES
-- TODO: Remove these
type TestPosition = At "a" 1  -- i.e. bottom left
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

type family UpdateBoard (board :: Board) (turn :: Team) (moves :: Moves) :: Board where
    UpdateBoard _ _ ('Moves VEnd) = TypeError (Text "A move must be made!")
    UpdateBoard _ _ _    = TypeError (Text "UpdateBoard is unfinished!")

type family IsUpdateValid (from :: Board) (to :: Board) (turn :: Team) :: Board where
    IsUpdateValid x x _ = TypeError (Text "A move must be made - the board cannot stay exactly the same.")
    IsUpdateValid _ _ _ = TypeError (Text "IsUpdateValid is unfinished!")

-- Rudimentary way to display type errors, for now.
x :: Proxy (UpdateBoard TestBoard White ('Moves VEnd))
x = Proxy

-- TODO: Check the piece's reported position is the actual position, eh
type family PieceMoves (p :: Piece) (board :: Board) :: Maybe (Vec n Position) where
    PieceMoves (MkPiece team Pawn info) board = PawnMoves (MkPiece team Pawn info) board
    PieceMoves _ _ = TypeError (Text "Unfinished!")

-- TODO: Include diagonal takes!
-- TODO: Take the board into account!
-- TODO: Return Nothing if no valid moves
type family PawnMoves (p :: Piece) (board :: Board) :: Maybe (Vec n Position) where
    PawnMoves (MkPiece Black Pawn (Info 0 (At col row))) board = Just (At col (row - 1) :<> At col (row - 2))
    PawnMoves (MkPiece Black Pawn (Info n (At col row))) board = Just (At col (row - 1) :-> VEnd)
    PawnMoves (MkPiece White Pawn (Info 0 (At col row))) board = Just (At col (row + 1) :<> At col (row + 2))
    PawnMoves (MkPiece White Pawn (Info n (At col row))) board = Just (At col (row + 1) :-> VEnd)

type family TestPawnMoves (p :: Piece) :: Maybe (Vec n Position) where
    -- TestPawnMoves (MkPiece Black Pawn (Info 0 (At col row))) = Just (At col (row - 1) :<> At col (row - 2))
    -- TestPawnMoves (MkPiece Black Pawn (Info m (At col row))) = Just (At col (row - 1) :-> VEnd)
    -- TestPawnMoves (MkPiece White Pawn (Info 0 (At col row))) = Just (At col (row + 1) :<> At col (row + 2))
    -- TestPawnMoves (MkPiece White Pawn (Info m (At col row))) = Just (At col (row + 1) :-> VEnd)
    TestPawnMoves p = Just (TestPosition :-> VEnd)
    -- TestPawnMoves (MkPiece Black Pawn (Info _ (At col row))) = Just (At col (row - 1) :-> VEnd)
    -- TestPawnMoves (MkPiece White Pawn (Info _ (At col row))) = Just (At col (row + 1) :-> VEnd)
    TestPawnMoves p = Nothing

type family TestPawnMoves2 (p :: PieceName) (t :: Team) (at :: Position) :: Vec n Position where
    TestPawnMoves2 Pawn Black (At col row) = (At col (row - 1)) :-> VEnd
    TestPawnMoves2 Pawn White (At col row) = (At col (row + 1)) :-> VEnd
    TestPawnMoves2 p t at = TypeError (Text "Bleh")

type family FromJust (x :: Maybe a) (y :: a) :: a where
    FromJust Nothing y  = y
    FromJust (Just x) _ = x

test :: Proxy (TestPawnMoves (MkPiece Black Pawn (Info 0 (At "a" 1))))
test = Proxy

vecTest :: Proxy (At "a" 2 :-> VEnd)
vecTest = Proxy @(TestPawnMoves2 Pawn White (At "a" 1))

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- type family Update (board :: Board) (pieces :: Vec n Piece) (positions :: Vec n Position) :: Board where
--     Update board pieces positions = TypeError (Text "Unfinished!")

-- -- TODO: Make this work
-- -- Associated type family for pieces that can move; takes their current position, the board,
-- -- and then a series of valid next positions!
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
