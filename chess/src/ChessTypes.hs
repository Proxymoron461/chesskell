module ChessTypes where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Vec
import Data.Type.Nat hiding (SNat(..))

-- Type synonym for an 8x8 grid
type Row = Vec Eight (Maybe Piece)
type Grid8x8 = Vec Eight Row

-- TODO: Dimensions of board in kind??
type Board = Grid8x8

data Piece where
    MkPiece :: Team -> PieceName -> PieceInfo -> Piece

data Team = Black | White

type instance TypeShow Black = "Black"
type instance TypeShow White = "White"

data OppositeTeam :: Team -> Exp Team
type instance Eval (OppositeTeam team) = OppositeTeam' team

type family OppositeTeam' (t :: Team) :: Team where
    OppositeTeam' White = Black
    OppositeTeam' Black = White

-- Make singleton types for each piece??
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
    Info :: Nat -> Position -> Bool -> PieceInfo

data GetMoveCount :: PieceInfo -> Exp Nat
type instance Eval (GetMoveCount (Info x _ _)) = x

data GetPosition :: PieceInfo -> Exp Position
type instance Eval (GetPosition (Info _ x _)) = x

data LastPieceToMoveInfo :: PieceInfo -> Exp Bool
type instance Eval (LastPieceToMoveInfo (Info _ _ x)) = x

-- TODO: Validity check??
data SetPosition :: PieceInfo -> Position -> Exp PieceInfo
type instance Eval (SetPosition (Info n _ x) pos) = Info n pos x

data InfoIncrementMoves :: PieceInfo -> Exp PieceInfo
type instance Eval (InfoIncrementMoves (Info n pos x)) = Info (S n) pos x

data IncrementMoves :: Piece -> Exp Piece
type instance Eval (IncrementMoves (MkPiece team name info)) = MkPiece team name (Eval (InfoIncrementMoves info))

data PieceMoveCount :: Piece -> Exp Nat
type instance Eval (PieceMoveCount (MkPiece _ _ info)) = Eval (GetMoveCount info)

data PiecePosition :: Piece -> Exp Position
type instance Eval (PiecePosition (MkPiece _ _ info)) = Eval (GetPosition info)

data LastPieceToMove :: Piece -> Exp Bool
type instance Eval (LastPieceToMove (MkPiece _ _ info)) = Eval (LastPieceToMoveInfo info)

data SetLastPieceToMove :: Piece -> Exp Piece
type instance Eval (SetLastPieceToMove (MkPiece team name (Info x y _))) = (MkPiece team name (Info x y True))

data ResetLastMoved :: Piece -> Exp Piece
type instance Eval (ResetLastMoved (MkPiece team name (Info x y _))) = (MkPiece team name (Info x y False))

data ResetLastPieceMoved :: Board -> Exp Board
type instance Eval (ResetLastPieceMoved board) = Eval ((Map (Map ResetLastMoved)) <$> board)

data SetLastPieceMoved :: Position -> Board -> Exp Board
type instance Eval (SetLastPieceMoved pos board) = Eval (ApplyFuncAt SetLastPieceToMove (Eval (ResetLastPieceMoved board)) pos)

data IsLastPieceMovedAt :: Position -> Board -> Exp Bool
type instance Eval (IsLastPieceMovedAt pos board) = Eval (FromMaybe False LastPieceToMove (Eval (GetPieceAt board pos)))

data SetPiecePosition :: Piece -> Position -> Exp Piece
type instance Eval (SetPiecePosition (MkPiece t n info) pos) = MkPiece t n (Eval (SetPosition info pos))

data PieceTeam :: Piece -> Exp Team
type instance Eval (PieceTeam (MkPiece team _ _)) = team

data PieceType :: Piece -> Exp PieceName
type instance Eval (PieceType (MkPiece _ name _)) = name

data NoOfPieces :: Board -> Exp Nat
type instance Eval (NoOfPieces board) = Eval (Foldr FCFPlus Nat0 (Eval ((VFilterCount IsJust) <$> board)))

data IsPiece :: PieceName -> Piece -> Exp Bool
type instance Eval (IsPiece name (MkPiece _ pcName _)) = Eval (name :==: pcName)

data IsPawn :: Piece -> Exp Bool
type instance Eval (IsPawn piece) = Eval (IsPiece Pawn piece)

data IsBishop :: Piece -> Exp Bool
type instance Eval (IsBishop piece) = Eval (IsPiece Bishop piece)

data IsKnight :: Piece -> Exp Bool
type instance Eval (IsKnight piece) = Eval (IsPiece Knight piece)

data IsRook :: Piece -> Exp Bool
type instance Eval (IsRook piece) = Eval (IsPiece Rook piece)

data IsKing :: Piece -> Exp Bool
type instance Eval (IsKing piece) = Eval (IsPiece King piece)

data IsQueen :: Piece -> Exp Bool
type instance Eval (IsQueen piece) = Eval (IsPiece Queen piece)

data Column = A | B | C | D | E | F | G | H
type instance TypeShow A = "A"
type instance TypeShow B = "B"
type instance TypeShow C = "C"
type instance TypeShow D = "D"
type instance TypeShow E = "E"
type instance TypeShow F = "F"
type instance TypeShow G = "G"
type instance TypeShow H = "H"

-- TODO: Type level char??
-- Goes column-row, e.g. At A 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Column -> Nat -> Position

type instance TypeShow (At col row) = "At " ++ TypeShow col ++ " (" ++ TypeShow row ++ ")"

type ValidRows = Nat1 :-> Nat2 :-> Nat3 :-> Nat4 :-> Nat5 :-> Nat6 :-> Nat7 :<> Nat8

-- TODO: Remove these, because now Column is a data type!
data ValidColumn :: Column -> Exp (Maybe Column)
type instance Eval (ValidColumn x) = Just x

data IsValidColumn :: Column -> Exp Bool
type instance Eval (IsValidColumn x) = True

data IsValidRow :: Nat -> Exp Bool
type instance Eval (IsValidRow x) = Eval (If (Elem x ValidRows) (ID True) (ID False))

data IsValidPosition :: Position -> Exp Bool
type instance Eval (IsValidPosition (At col row)) = Eval ((Eval (IsValidColumn col)) :&&: (IsValidRow row))

-- This checks for the validity of the position before it sends one off!
data GetPieceAt :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAt board pos) = Eval (If (Eval (IsValidPosition pos)) (GPANCUgly board pos) (ID Nothing))

data GetPieceAtNoChecks :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAtNoChecks board (At col row)) = Eval (Join (Eval (Join (Eval ((Eval ((CW (!!)) <$> (Eval (GetRow board row)))) <*> (Just ((ColToIndex col))))))))

type family FromJust' (x :: Maybe a) :: a where
    FromJust' (Just x) = x

data GPANCUgly :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GPANCUgly (a :-> xs) (At col Nat1)) = VAUgly a (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c) (At col Nat2)) = VAUgly b (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d) (At col Nat3)) = VAUgly c (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e) (At col Nat4)) = VAUgly d (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f) (At col Nat5)) = VAUgly e (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g) (At col Nat6)) = VAUgly f (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) (At col Nat7)) = VAUgly g (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> xs) (At col Nat8)) = VAUgly h (ColToIndex col)

data GetPieceAtWhich :: Board -> Position -> (a -> Exp Bool) -> Exp (Maybe Piece)
type instance Eval (GetPieceAtWhich board pos f) = Eval (MaybeWhich f (Eval (GetPieceAt board pos)))

data IsPieceAtWhich :: Board -> Position -> (a -> Exp Bool) -> Exp Bool
type instance Eval (IsPieceAtWhich board pos f) = Eval ((IsJust . (GetPieceAtWhich board pos)) f)

data ApplyFuncAt :: (Piece -> Exp Piece) -> Board -> Position -> Exp Board
type instance Eval (ApplyFuncAt f board pos) = Eval (FromMaybe board ((FlipToLast SetPieceAt) board pos . f) (Eval (GetPieceAt board pos)))

data SetPieceAt :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAt piece board pos) = Eval (If (Eval (IsValidPosition pos)) (SetPieceAtNoChecks piece board pos) (ID board))
data SetPieceAtSwapped :: Piece -> Position -> Board -> Exp Board
type instance Eval (SetPieceAtSwapped piece pos board) = Eval (SetPieceAt piece board pos)

data ClearPieceAt :: Position -> Board -> Exp Board
type instance Eval (ClearPieceAt (At col row) board) = Eval (SetRow board row (Eval (PutAt Nothing (ColToIndex col) (Eval (FromJust (Eval (GetRow board row)))))))

data SetPieceAtNoChecks :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAtNoChecks piece board (At col row)) = Eval (SetRow board row (Eval (PutAt (Just (Eval (SetPiecePosition piece (At col row)))) (ColToIndex col) (Eval (FromJust (Eval (GetRow board row)))))))

-- TODO: Optimise to work in one fell swoop, rather than one by one?
data SetPiecesAt :: [(Piece, Position)] -> Board -> Exp Board
type instance Eval (SetPiecesAt pps board) = Eval (Foldr (Uncurry2 SetPieceAtSwapped) board pps)

data GetRow :: Board -> Nat -> Exp (Maybe Row)
type instance Eval (GetRow board (S n)) = Just $ VAUgly board n

-- Uses 1 for first row, and 8 for last row!
data SetRow :: Board -> Nat -> Row -> Exp Board
type instance Eval (SetRow board (S n) row) = Eval (PutAt row n board)

-- Type families to add an offset to columns!
-- TODO: Customise the number of columns?? As it is, it's chess-specific.
-- TODO: Flip the arguments, they're the wrong way round!!
data (:+) :: Nat -> Column -> Exp (Maybe Column)
data (:-) :: Nat -> Column -> Exp (Maybe Column)

type instance Eval ((:+) Z         col) = Eval (ValidColumn col)
type instance Eval ((:+) (S Z)     A) = Just B
type instance Eval ((:+) (S Z)     B) = Just C
type instance Eval ((:+) (S Z)     C) = Just D
type instance Eval ((:+) (S Z)     D) = Just E
type instance Eval ((:+) (S Z)     E) = Just F
type instance Eval ((:+) (S Z)     F) = Just G
type instance Eval ((:+) (S Z)     G) = Just H
type instance Eval ((:+) (S Z)     H) = Nothing
type instance Eval ((:+) (S (S n)) col) = Eval (Bind ((:+) (S n)) (Eval ((:+) (S Z) col)))

type instance Eval ((:-) Z         col) = Eval (ValidColumn col)
type instance Eval ((:-) (S Z)     A) = Nothing
type instance Eval ((:-) (S Z)     B) = Just A
type instance Eval ((:-) (S Z)     C) = Just B
type instance Eval ((:-) (S Z)     D) = Just C
type instance Eval ((:-) (S Z)     E) = Just D
type instance Eval ((:-) (S Z)     F) = Just E
type instance Eval ((:-) (S Z)     G) = Just F
type instance Eval ((:-) (S Z)     H) = Just G
type instance Eval ((:-) (S (S n)) col) = Eval (Bind ((:-) (S n)) (Eval ((:-) (S Z) col)))

-- TODO: Maybe make this tied less to ValidColumns??
type family ColToIndex (col :: Column) :: Nat where
    -- ColToIndex col = ElemIndex ValidColumns col
    ColToIndex A = Nat0
    ColToIndex B = Nat1
    ColToIndex C = Nat2
    ColToIndex D = Nat3
    ColToIndex E = Nat4
    ColToIndex F = Nat5
    ColToIndex G = Nat6
    ColToIndex H = Nat7

-----------------------------------------------------------------------------------------------------

type LastMoveInfo = (Maybe Board, Team)

-- type StartBoard = Eval (SetPiecesAt '[ '(MkPiece Black King MyTestInfo, At A Nat1), '(MkPiece White King MyTestInfo, At H Nat8), '(MkPiece Black Queen MyTestInfo, At D Nat3), '(MkPiece White Queen MyTestInfo, At E Nat3)] EmptyBoard)
type StartInfo = 'Info Z (At A Z) False

type family RowPositions (n :: TL.Nat) :: [Position] where
    RowPositions n = RowPositionsDTNat (FromGHC n)

type family RowPositionsDTNat (n :: Nat) :: [Position] where
    RowPositionsDTNat n = '[ At A n, At B n, At C n, At D n, At E n, At F n, At G n, At H n]

-- Left-to-right construction of a row of pieces, given the team
type family PieceRow (t :: Team) :: [Piece] where
    PieceRow t = ('[
        MkPiece t Rook StartInfo,
        MkPiece t Knight StartInfo,
        MkPiece t Bishop StartInfo,
        MkPiece t Queen StartInfo,
        MkPiece t King StartInfo,
        MkPiece t Bishop StartInfo,
        MkPiece t Knight StartInfo,
        MkPiece t Rook StartInfo ])

-- FIXME: Horrible memory usage. Just set this out manually?
type StartBoard = Eval (SetPiecesAt (
  Eval (Zip (PieceRow Black) (RowPositions 8))
  ++ Eval (Zip (Eval (Replicate 8 (MkPiece Black Pawn StartInfo))) (RowPositions 7))
  ++ Eval (Zip (Eval (Replicate 8 (MkPiece White Pawn StartInfo))) (RowPositions 2))
  ++ Eval (Zip (PieceRow White) (RowPositions 1))) EmptyBoard)

type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type EmptyBoard = EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :<> EmptyRow


-- -- data SetRow :: Board -> Nat -> Row -> Exp Board
-- type StartBoard = (EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow
--     :-> EmptyRow)