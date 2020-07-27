module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- TODO: Dimensions of board in type??
data Board where
    MkBoard :: [[Piece]] -> Board

data Piece where
    MkPiece :: Team -> PieceName -> PieceInfo -> Piece

data Team = Black | White

data PieceName = Pawn
               | Bishop
               | Knight
               | Rook
               | King
               | Queen

data PieceInfo where
    Info :: (Nat, Position)

data Position where
    At :: Char -> Nat

data Proxy a = Proxy

type family Move (board :: Board) (pieces :: '[Piece]) (positions :: '[Position]) :: Board where
    Move board pieces positions = TypeError (Text "Unfinished!")

type family IsMoveValid (from :: Board) (to :: Board) :: Board where
    IsMoveValid _ _ = TypeError (Text "Unfinished!")

-- type family All (c :: * -> Constraint) (ts :: [*]) :: Constraint where
--     All c '[]       = ()
--     All c (t ': ts) = (c t, All c ts)

-- type family Or (a :: Bool) (b :: Bool) :: Bool where
--     Or 'False b      = b
--     Or 'True  _      = 'True

-- type family Some (c :: * -> Constraint) (ts :: [*]) :: Bool where
--     Some c '[]       = 'False
--     Some c (t ': ts) = Or (c t) (Some c ts)

-- type family Any (c :: * -> Constraint) (ts :: [*]) :: Constraint where
--     Any c ts = (Some c ts) ~ 'True

-- type family ValidPosStr (s :: Symbol) :: Constraint where
--     ValidPosStr s = Any (~ s) '[ 'a','b','c','d','e','f','g','h' ]
