module Lib where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Vec
import ChessTypes
import Data.Type.Nat hiding (SNat(..))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GetAllBelow :: Position -> Exp [Position]
type instance Eval (GetAllBelow (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetweenDTNat row Nat0)))))

data GetNBelow :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNBelow n (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetweenDTNat row (row - (FromGHC n)))))))

-- Takes in x and y, and performs x - y with a lower bound of 0
data SafeMinus :: TL.Nat -> TL.Nat -> Exp TL.Nat
type instance Eval (SafeMinus x y) = Eval (SafeMinusHelper x y (TL.CmpNat x y))

data SafeMinusHelper :: TL.Nat -> TL.Nat -> Ordering -> Exp TL.Nat
type instance Eval (SafeMinusHelper x y LT) = 0
type instance Eval (SafeMinusHelper x y EQ) = 0
type instance Eval (SafeMinusHelper x y GT) = x TL.- y

-- Generates a range between two TL.Nat values, non-inclusive of the first argument
data RangeBetween :: TL.Nat -> TL.Nat -> Exp [TL.Nat]
type instance Eval (RangeBetween n m) = Eval (RangeBetweenHelper n m (TL.CmpNat n m))

data RangeBetweenHelper :: TL.Nat -> TL.Nat -> Ordering -> Exp [TL.Nat]
type instance Eval (RangeBetweenHelper n m LT) = (n TL.+ 1) ': (Eval (RangeBetweenHelper (n TL.+ 1) m (TL.CmpNat (n TL.+ 1) m)))
type instance Eval (RangeBetweenHelper n m EQ) = '[]
type instance Eval (RangeBetweenHelper n m GT) = (n TL.- 1) ': (Eval (RangeBetweenHelper (n TL.- 1) m (TL.CmpNat (n TL.- 1) m)))

data RangeBetweenDTNat :: Nat -> Nat -> Exp [Nat]
type instance Eval (RangeBetweenDTNat n m) = Eval (Map TLNatToNat (Eval (RangeBetween (ToGHC n) (ToGHC m))))

-- -- Generates a range between two Char values, non-inclusive of the first argument
-- -- It will only go from lowercase "a" to lowercase "z"
-- data CharRangeBetween :: TL.Symbol -> TL.Symbol -> Exp [TL.Symbol]
-- type instance Eval (CharRangeBetween a b) = Eval (CharRangeBetweenHelper a b (CmpTL.Symbol a b))

-- data CharRangeBetweenHelper :: TL.Symbol -> TL.Symbol -> Ordering -> Exp [TL.Symbol]
-- type instance Eval (CharRangeBetweenHelper a b LT) = Eval (TakeWhile (CharLessThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:+)) a) (Eval (RangeBetweenDTNat 0 8)))))))))
-- type instance Eval (CharRangeBetweenHelper a b EQ) = '[]
-- type instance Eval (CharRangeBetweenHelper a b GT) = Eval (TakeWhile (CharGreaterThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:-)) a) (Eval (RangeBetweenDTNat 0 8)))))))))

-- data CharLessThan :: TL.Symbol -> TL.Symbol -> Exp Bool
-- type instance Eval (CharLessThan b a) = Eval (IsLTEQ (CmpTL.Symbol a b))

-- data CharGreaterThan :: TL.Symbol -> TL.Symbol -> Exp Bool
-- type instance Eval (CharGreaterThan b a) = Eval (IsGTEQ (CmpTL.Symbol a b))

-- Generates a range between two Column values, non-inclusive of the first argument
data ColRangeBetween :: Column -> Column -> Exp [Column]
type instance Eval (ColRangeBetween a b) = Eval (ColRangeBetweenHelper a b (CmpColumn a b))

data ColRangeBetweenHelper :: Column -> Column -> Ordering -> Exp [Column]
type instance Eval (ColRangeBetweenHelper a b LT) = Eval (TakeWhile (ColLessThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:+)) a) (Eval (RangeBetweenDTNat Nat0 Nat8)))))))))
type instance Eval (ColRangeBetweenHelper a b EQ) = '[]
type instance Eval (ColRangeBetweenHelper a b GT) = Eval (TakeWhile (ColGreaterThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:-)) a) (Eval (RangeBetweenDTNat Nat0 Nat8)))))))))

data ColLessThan :: Column -> Column -> Exp Bool
type instance Eval (ColLessThan b a) = Eval (IsLTEQ (CmpColumn a b))

data ColGreaterThan :: Column -> Column -> Exp Bool
type instance Eval (ColGreaterThan b a) = Eval (IsGTEQ (CmpColumn a b))

type family CmpColumn (x :: Column) (y :: Column) :: Ordering where
    CmpColumn A A = EQ
    CmpColumn A B = LT
    CmpColumn A C = LT
    CmpColumn A D = LT
    CmpColumn A E = LT
    CmpColumn A F = LT
    CmpColumn A G = LT
    CmpColumn A H = LT
    CmpColumn B A = GT
    CmpColumn C A = GT
    CmpColumn D A = GT
    CmpColumn E A = GT
    CmpColumn F A = GT
    CmpColumn G A = GT
    CmpColumn H A = GT
    CmpColumn x y = CmpColumn (ColDec x) (ColDec y)

-- Type family to decrement a column value!
type family ColDec (x :: Column) :: Column where
    ColDec A = A
    ColDec B = A
    ColDec C = B
    ColDec D = C
    ColDec E = D
    ColDec F = E
    ColDec G = F
    ColDec H = G

data IsLTEQ :: Ordering -> Exp Bool
type instance Eval (IsLTEQ LT) = True
type instance Eval (IsLTEQ EQ) = True
type instance Eval (IsLTEQ GT) = False

data IsGTEQ :: Ordering -> Exp Bool
type instance Eval (IsGTEQ LT) = False
type instance Eval (IsGTEQ EQ) = True
type instance Eval (IsGTEQ GT) = True

data GetAllAbove :: Position -> Exp [Position]
type instance Eval (GetAllAbove (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetweenDTNat row Nat8)))))

data GetNAbove :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNAbove n (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetweenDTNat row (row + (FromGHC n)))))))

-- (:+) :: Nat -> Column -> Exp (Maybe Column)
data GetNRight :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNRight n pos) = Eval (Filter IsValidPosition (Eval (GetNRightPositionsNoChecks n pos)))

-- TODO: Combine with GetNLeftMaybes to achieve DRY?
data GetNRightMaybes :: TL.Nat -> Position -> Exp [Maybe Column]
type instance Eval (GetNRightMaybes n (At col row)) = Eval (Filter IsJust (Eval (Map ((Flip (:+)) col) (Eval (Map TLNatToNat (Eval (RangeBetween 0 n)))))))

-- TODO: Combine with GetNLeftPositionsNoChecks to achieve DRY?
data GetNRightPositionsNoChecks :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNRightPositionsNoChecks n (At col row)) = Eval (Map (((Flip (CW2 At)) row) . FromJust) (Eval (GetNRightMaybes n (At col row))))

data GetAllRight :: Position -> Exp [Position]
type instance Eval (GetAllRight pos) = Eval (GetNRight 8 pos)

data GetNLeft :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNLeft n pos) = Eval (Filter IsValidPosition (Eval (GetNLeftPositionsNoChecks n pos)))

-- Can reverse with RangeBetweenDTNat (n + 1) 1
data GetNLeftMaybes :: TL.Nat -> Position -> Exp [Maybe Column]
type instance Eval (GetNLeftMaybes n (At col row)) = Eval (Filter IsJust (Eval (Map ((Flip (:-)) col) (Eval (Map TLNatToNat (Eval (RangeBetween 0 n)))))))

data GetNLeftPositionsNoChecks :: TL.Nat -> Position -> Exp [Position]
type instance Eval (GetNLeftPositionsNoChecks n (At col row)) = Eval (Map (((Flip (CW2 At)) row) . FromJust) (Eval (GetNLeftMaybes n (At col row))))

data GetAllLeft :: Position -> Exp [Position]
type instance Eval (GetAllLeft pos) = Eval (GetNLeft 8 pos)

data GetAllDiagNW :: Position -> Exp [Position]
type instance Eval (GetAllDiagNW (At col row)) = Eval (ZipWith (Eval (ColRangeBetween col H)) (Eval (RangeBetweenDTNat row Nat8)) (CW2 At))

data GetAllDiagSW :: Position -> Exp [Position]
type instance Eval (GetAllDiagSW (At col row)) = Eval (ZipWith (Eval (ColRangeBetween col H)) (Eval (RangeBetweenDTNat row Nat1)) (CW2 At))

data GetAllDiagSE :: Position -> Exp [Position]
type instance Eval (GetAllDiagSE (At col row)) = Eval (ZipWith (Eval (ColRangeBetween col A)) (Eval (RangeBetweenDTNat row Nat1)) (CW2 At))

data GetAllDiagNE :: Position -> Exp [Position]
type instance Eval (GetAllDiagNE (At col row)) = Eval (ZipWith (Eval (ColRangeBetween col A)) (Eval (RangeBetweenDTNat row Nat8)) (CW2 At))

data GetAllKnightPositions :: Position -> Exp [Position]
type instance Eval (GetAllKnightPositions pos) = Eval (Filter IsValidPosition (Eval (GetKnightAboveBelow pos) ++ Eval (GetKnightLeftRight pos)))

data GetKnightAboveBelow :: Position -> Exp [Position]
type instance Eval (GetKnightAboveBelow (At col row)) = Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 1)) <*> Eval (GetKnightRows row Nat2))

data GetKnightLeftRight :: Position -> Exp [Position]
type instance Eval (GetKnightLeftRight (At col row)) = Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 2)) <*> Eval (GetKnightRows row Nat1))

data GetKnightColumns :: Column -> TL.Nat -> Exp [Column]
type instance Eval (GetKnightColumns col n) = Eval (GetKnightColumnsNat col (Eval (TLNatToNat n)))

data GetKnightColumnsNat :: Column -> Nat -> Exp [Column]
type instance Eval (GetKnightColumnsNat col n) = Eval (FilterMap (IsJust) (FromJust) '[ Eval (n :+ col), Eval (n :- col) ])

data GetKnightRows :: Nat -> Nat -> Exp [Nat]
type instance Eval (GetKnightRows row n) = Eval (If (n <=? row) (ID '[ row - n, row + n]) (ID '[ row + n ]))

-- :kind! Eval ((Eval ((CW FCFPlus) <$> [2,1,0])) <*> [1,2,3])
-- NOTE: Uses Tail to remove the current position!
data GetAdjacent :: Position -> Exp [Position]
type instance Eval (GetAdjacent (At col row)) = Eval (Filter IsValidPosition (Eval (Tail (Eval ((Eval (CW (CW2 At) <$> (Eval (GetAdjacentColumns col)))) <*> '[row, row + Nat1, row - Nat1])))))

data GetAdjacentColumns :: Column -> Exp [Column]
type instance Eval (GetAdjacentColumns col) = col ': Eval (Map FromJust (Eval (Filter IsJust '[Eval ((S Z) :+ col), Eval ((S Z) :- col)])))

data IsOpposingTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece White _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece Black _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece Black _ _)) = True
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece White _ _)) = True

data OppositeTeam :: Team -> Exp Team
type instance Eval (OppositeTeam White) = Black
type instance Eval (OppositeTeam Black) = White

data IsSameTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsSameTeam p1 p2) = Eval ((Not . (IsOpposingTeam p1)) p2)

data HasTeam :: Team -> Piece -> Exp Bool
type instance Eval (HasTeam White (MkPiece White _ _)) = True
type instance Eval (HasTeam Black (MkPiece Black _ _)) = True
type instance Eval (HasTeam White (MkPiece Black _ _)) = False
type instance Eval (HasTeam Black (MkPiece White _ _)) = False

data HasRow :: Nat -> Position -> Exp Bool
type instance Eval (HasRow x (At _ y)) = Eval ((x <=? y) :&&: ID (y <=? x))

-- Type families for getting all available squares in a straight line, with nothing in the way
data AllReachableFunc :: Team -> Board -> Position -> (Position -> Exp [Position]) -> Exp [Position]
type instance Eval (AllReachableFunc team board pos f) = Eval (TakeWhilePlus (Not . (IsPieceAt board)) ((MaybeIf (Not . (HasTeam team))) . (GetPieceAt board)) (Eval (f pos)))

data AllReachableLeft :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableLeft team board pos) = Eval (AllReachableFunc team board pos GetAllLeft)

data AllReachableRight :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableRight team board pos) = Eval (AllReachableFunc team board pos GetAllRight)

data AllReachableAbove :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableAbove team board pos) = Eval (AllReachableFunc team board pos GetAllAbove)

data AllReachableBelow :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableBelow team board pos) = Eval (AllReachableFunc team board pos GetAllBelow)

data AllReachableStraightLine :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableStraightLine team board pos) = Eval (Concat (Eval (Map (AllReachableFunc team board pos) '[ GetAllLeft, GetAllRight, GetAllAbove, GetAllBelow ])))

data AllReachableLineAndDiag :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableLineAndDiag team board pos) = (Eval (AllReachableStraightLine team board pos)) ++ (Eval (AllReachableDiag team board pos))

-- Reachable square type families for all diagonal directions at once: helpful
-- for Bishops and Queens!
data AllReachableDiag :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiag team board pos) = Eval (Concat (Eval (Map (AllReachableFunc team board pos) '[ GetAllDiagNW, GetAllDiagSW, GetAllDiagSE, GetAllDiagNE ])))

-- Reachable square type families for each diagonal direction
data AllReachableDiagNW :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNW team board pos) = Eval (AllReachableFunc team board pos GetAllDiagNW)

data AllReachableDiagSW :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSW team board pos) = Eval (AllReachableFunc team board pos GetAllDiagSW)

data AllReachableDiagSE :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSE team board pos) = Eval (AllReachableFunc team board pos GetAllDiagSE)

data AllReachableDiagNE :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNE team board pos) = Eval (AllReachableFunc team board pos GetAllDiagNE)

-- Prunes a list for all spaces taken up by a piece of the same team
-- (Perfect for kinds and knights!)
data AllReachableGivenList :: Team -> Board -> [Position] -> Exp [Position]
type instance Eval (AllReachableGivenList team board list) = Eval (Filter (FromMaybe True (Not . HasTeam team) . GetPieceAt board) list)

-- General function, for taking the first N reachable positions from a particular direction.
-- NOTE: Relies on each directional function giving them in order of distance from the player
-- NOTE: Does not work with AllReachableDiag, as that will only be in one direction.
data NReachableFunc :: Team -> Board -> Position -> (Team -> Board -> Position -> Exp [Position]) -> TL.Nat -> Exp [Position]
type instance Eval (NReachableFunc team board pos f n) = Eval (Take n (Eval (f team board pos)))

data NReachableDiagNW :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagNW team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagNW n)

data NReachableDiagNE :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagNE team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagNE n)

data NReachableDiagSW :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagSW team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagSW n)

data NReachableDiagSE :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagSE team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagSE n)

data NReachableBelow :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableBelow team board pos n) = Eval (NReachableFunc team board pos AllReachableBelow n)

data NReachableAbove :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableAbove team board pos n) = Eval (NReachableFunc team board pos AllReachableAbove n)

data NReachableLeft :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableLeft team board pos n) = Eval (NReachableFunc team board pos AllReachableLeft n)

data NReachableRight :: Team -> Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableRight team board pos n) = Eval (NReachableFunc team board pos AllReachableRight n)

data GetOneLeft :: Position -> Exp (Maybe Position)
type instance Eval (GetOneLeft (At col row)) = Eval (Eval ((S Z) :- col) >>= ((CW Just) . ((Flip (CW2 At)) row)))

data GetOneRight :: Position -> Exp (Maybe Position)
type instance Eval (GetOneRight (At col row)) = Eval (Eval ((S Z) :+ col) >>= ((CW Just) . ((Flip (CW2 At)) row)))

-- The pawn is the only piece whose attack rules differ from its' movement rules;
-- so it requires a special case.
data PawnReachableAbove :: Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (PawnReachableAbove board pos n) = Eval (GetFreePositions (Eval (NReachableAbove White board pos n)) board)

data PawnReachableBelow :: Board -> Position -> TL.Nat -> Exp [Position]
type instance Eval (PawnReachableBelow board pos n) = Eval (GetFreePositions (Eval (NReachableBelow Black board pos n)) board)

data IsPieceAt :: Board -> Position -> Exp Bool
type instance Eval (IsPieceAt board pos) = Eval (IsJust (Eval (GetPieceAt board pos)))

data IsKingAt :: Team -> Board -> Position -> Exp Bool
type instance Eval (IsKingAt team board pos) = Eval (FromMaybe False (HasTeam team .&. IsKing) (Eval (GetPieceAt board pos)))

data IsQueenAt :: Board -> Position -> Exp Bool
type instance Eval (IsQueenAt board pos) = Eval (FromMaybe False IsQueen (Eval (GetPieceAt board pos)))

data GetFreePositions :: [Position] -> Board -> Exp [Position]
type instance Eval (GetFreePositions '[] _) = '[]
type instance Eval (GetFreePositions (p ': ps) board) = Eval (If (Eval ((Eval (IsPieceAt board p)) :||: ((Not . IsValidPosition) p))) (GetFreePositions ps board) (ID (p ': (Eval (GetFreePositions ps board)))))

-- :k Foldr :: (Row -> [Pos] -> Exp [Pos]) -> [Pos] -> [Row] -> Exp [Pos]
data GetUnderAttackPositions :: Team -> Board -> Exp [Position]
type instance Eval (GetUnderAttackPositions team board) = Eval (Foldr (AddRowMovesToList team board) '[] board)

data AddRowMovesToList :: Team -> Board -> Row -> [Position] -> Exp [Position]
type instance Eval (AddRowMovesToList team board row list) = Eval (GetRowUnderAttackPositions team board row) ++ list

-- :k Foldr :: (Maybe Piece -> [Pos] -> Exp [Pos]) -> [Pos] -> [Maybe Piece] -> Exp [Pos]
data GetRowUnderAttackPositions :: Team -> Board -> Row -> Exp [Position]
type instance Eval (GetRowUnderAttackPositions team board row) = Eval (Foldr (AddMovesToList team board) '[] row)

data AddMovesToList :: Team -> Board -> Maybe Piece -> [Position] -> Exp [Position]
type instance Eval (AddMovesToList team board maybePiece list) = Eval (FromMaybe '[] ((Flip PieceAttackList) board) (Eval (If (Eval (MaybeIf (HasTeam team) maybePiece)) (ID maybePiece) (ID Nothing)))) ++ list

type family NoKingError (team :: Team) where
    NoKingError team = TL.TypeError (TL.Text ("There is no " ++ TypeShow team ++ " King on the board!"))

data FindKing :: Team -> Board -> Exp Piece
type instance Eval (FindKing team board) = Eval (FromMaybe (NoKingError team) FromJust (Eval (Find IsJust (Eval (FindKingInRow team <$> board)))))

data FindKingInRow :: Team -> Row -> Exp (Maybe Piece)
type instance Eval (FindKingInRow team row) = Eval (Join (Eval (Find (MaybeIf (IsKing .&. HasTeam team)) row)))

data FindKingPosition :: Team -> Board -> Exp Position
type instance Eval (FindKingPosition team board) = Eval (PiecePosition (Eval (FindKing team board)))

data IsKingInCheck :: Team -> Board -> Exp Bool
-- type instance Eval (IsKingInCheck team board) = Eval (Eval (FindKingPosition team board) `In` Eval (GetUnderAttackPositions (Eval (OppositeTeam team)) board))
type instance Eval (IsKingInCheck team board) = Eval (Any (IsKingAt team board) (Eval (GetUnderAttackPositions (Eval (OppositeTeam team)) board)))
-- type instance Eval (IsKingInCheck team board) = False  -- Proves that this function is a major bottleneck

-- This function just checks the spots a piece can move to; it does not handle moving itself.
-- That is in the other function, named Move.
-- Returns an empty list if the board is empty at that position!
-- NOTE: This allows pieces to state that they can move to the King's position; but this is just for check purposes. They can't actually take the king.
data CalculateValidMoves :: Position -> Board -> Exp [Position]
type instance Eval (CalculateValidMoves pos board) = Eval (FromMaybe '[] ((Flip PieceMoveList) board) (Eval (GetPieceAt board pos)))

-- TODO: Check that the piece's reported position is its' actual position
-- TODO: Test all this!!! Near-urgently!
data PieceMoveList :: Piece -> Board -> Exp [Position]
type instance Eval (PieceMoveList (MkPiece team Pawn info) board)   = Eval (If (Eval ((IsZero . GetMoveCount) info)) (PawnStartMove (MkPiece team Pawn info) board) (PawnPostStart (MkPiece team Pawn info) board))
type instance Eval (PieceMoveList (MkPiece team Bishop info) board) = Eval (AllReachableDiag team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Knight info) board) = Eval (AllReachableGivenList team board (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceMoveList (MkPiece team Rook info) board)   = Eval (AllReachableStraightLine team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Queen info) board)  = Eval (AllReachableLineAndDiag team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team King info) board)   = Eval (AllReachableGivenList team board (Eval (GetAdjacent (Eval (GetPosition info)))))

data PieceAttackList :: Piece -> Board -> Exp [Position]
type instance Eval (PieceAttackList (MkPiece team Pawn info) board)   = Eval (PawnTakePositions (MkPiece team Pawn info) board)
type instance Eval (PieceAttackList (MkPiece team Bishop info) board) = Eval (AllReachableDiag team board (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team Knight info) board) = Eval (AllReachableGivenList team board (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceAttackList (MkPiece team Rook info) board)   = Eval (AllReachableStraightLine team board (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team Queen info) board)  = Eval (AllReachableLineAndDiag team board (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team King info) board)   = Eval (AllReachableGivenList team board (Eval (GetAdjacent (Eval (GetPosition info)))))

-- First boolean argument determines the reach - the King check occurs if it is true,
-- and it does not if it is False.
data PieceCanReachKingCheck :: Bool -> Piece -> Position -> Board -> Exp Bool
type instance Eval (PieceCanReachKingCheck True piece pos board) = Eval (Eval (pos `In` Eval (PieceMoveList piece board)) :&&: ((Not . IsKingAt (Eval (OppositeTeam (Eval (PieceTeam piece)))) board) pos))
type instance Eval (PieceCanReachKingCheck False piece pos board) = Eval (pos `In` Eval (PieceMoveList piece board))

data PieceCanMoveTo :: Piece -> Position -> Board -> Exp Bool
type instance Eval (PieceCanMoveTo piece pos board) = Eval (PieceCanReachKingCheck True piece pos board)

data CanMoveTo :: Position -> Position -> Board -> Exp Bool
type instance Eval (CanMoveTo fromPos toPos board) = Eval (FromMaybe False ((FlipToLast PieceCanMoveTo) toPos board) (Eval (GetPieceAt board fromPos)))

data PieceCanReach :: Piece -> Position -> Board -> Exp Bool
type instance Eval (PieceCanReach piece pos board) = Eval (PieceCanReachKingCheck False piece pos board)

data CanReach :: Position -> Position -> Board -> Exp Bool
type instance Eval (CanReach fromPos toPos board) = Eval (FromMaybe False ((FlipToLast PieceCanReach) toPos board) (Eval (GetPieceAt board fromPos)))

-- Type family for where a pawn can move when it is in its' starting position
-- TODO: Throw a type error if the Pawn has already moved??
data PawnStartMove :: Piece -> Board -> Exp [Position]
type instance Eval (PawnStartMove pawn board) = (Eval (PawnMove pawn board 2)) ++ (Eval (PawnTakePositions pawn board))

-- Type family for getting the initial pawn two-forward move!
data PawnMove :: Piece -> Board -> TL.Nat -> Exp [Position]
type instance Eval (PawnMove (MkPiece Black Pawn info) board n) = Eval (PawnReachableBelow board (Eval (GetPosition info)) n)
type instance Eval (PawnMove (MkPiece White Pawn info) board n) = Eval (PawnReachableAbove board (Eval (GetPosition info)) n)

-- Pawns can take diagonally in front of themselves: so this gets those positions if a take is possible!
-- TODO: Handle "en passant" takes
data PawnTakePositions :: Piece -> Board -> Exp [Position]
type instance Eval (PawnTakePositions (MkPiece Black Pawn info) board) = (Eval (NReachableDiagSE Black board (Eval (GetPosition info)) 1)
    ++ (Eval (NReachableDiagSW Black board (Eval (GetPosition info)) 1))
    ++ (Eval (GetEnPassantPositions (MkPiece Black Pawn info) board)))
type instance Eval (PawnTakePositions (MkPiece White Pawn info) board) = (Eval (NReachableDiagNE White board (Eval (GetPosition info)) 1)
    ++ (Eval (NReachableDiagNW White board (Eval (GetPosition info)) 1))
    ++ (Eval (GetEnPassantPositions (MkPiece White Pawn info) board)))

data GetEnPassantPositions :: Piece -> Board -> Exp [Position]
type instance Eval (GetEnPassantPositions (MkPiece team name info) board) = Eval (Filter (IsSpaceVulnerableToEnPassant team board) (Eval (GetLeftRightPositions (Eval (GetPosition info)))))

data GetLeftRightPositions :: Position -> Exp [Position]
type instance Eval (GetLeftRightPositions pos) = Eval (FilterMap IsJust FromJust (Eval ('[GetOneLeft, GetOneRight] <*> '[ pos ])))

-- TODO: Ensure that this is only valid on the move immediately after that pawn moves
data IsSpaceVulnerableToEnPassant :: Team -> Board -> Position -> Exp Bool
type instance Eval (IsSpaceVulnerableToEnPassant team board pos) = Eval (FromMaybe False PawnMovedTwoLast (Eval (GetPieceAtWhich board pos (IsPawn .&. HasTeam (Eval (OppositeTeam team))))))

data PawnMovedTwoLast :: Piece -> Exp Bool
type instance Eval (PawnMovedTwoLast piece) = Eval (If (Eval (HasTeam White piece)) 
    (Eval (LastPieceToMove piece) :&&: (((HasRow Nat4 . PiecePosition) .&. (Equal (S Z) . PieceMoveCount)) piece))
    (Eval (LastPieceToMove piece) :&&: (((HasRow Nat5 . PiecePosition) .&. (Equal (S Z) . PieceMoveCount)) piece)))

data PawnPostStart :: Piece -> Board -> Exp [Position]
type instance Eval (PawnPostStart pawn board) = (Eval (PawnMove pawn board 1)) ++ (Eval (PawnTakePositions pawn board))


-- TODO: Scrap "Maybe Board" and just use board or type error??
data IfValidThenMove :: Team -> Position -> Position -> Board -> Exp (Maybe Board)
type instance Eval (IfValidThenMove team fromPos toPos board)
    = Eval (If (Eval (IsPieceAtWhich board fromPos (HasTeam team))) (Move fromPos toPos board) (TE' (TL.Text ("There is no valid move from: " ++ TypeShow fromPos ++ " to: " ++ TypeShow toPos))))

-- Type family for actually moving the piece, and handling the side effects.
-- TODO: Make sure that no moves allow you to stay in place
-- TODO: Handle moves that can transform pieces (e.g. Pawn moving to the edge of the board)
-- TODO: Handle takes (i.e. moves that remove pieces from play)
-- TODO: Ensure no moves place that piece's King into check
-- TODO: Ensure that if the King is in check, the next move takes him out of it
-- TODO: Move the piece/pieces, update those pieces' position info, increment those pieces' move count
-- TODO: Ensure that the moves go Black, White, Black, White...
data Move :: Position -> Position -> Board -> Exp (Maybe Board)
type instance Eval (Move fromPos toPos board) = Eval (If (Eval (CanMoveTo fromPos toPos board)) (MoveNoChecks fromPos toPos board) (TE' (TL.Text ("There is no valid move from: " ++ TypeShow fromPos ++ " to: " ++ TypeShow toPos))))

data MoveNoChecks :: Position -> Position -> Board -> Exp (Maybe Board)
type instance Eval (MoveNoChecks fromPos toPos board) = Eval (ClearPieceAt fromPos <$> (Eval (Eval (GetPieceAt board fromPos) >>= (FlipToLast MovePiece) toPos board)))

-- Does not check that it's valid the piece can move to the position, just moves them!
-- For all pieces apart from the King and the Pawn, moving them is easy: they move
-- a single piece, and have no side effects.
-- But the King can castle, and the Pawn can do en passant and turn into a Queen!
-- Very complicated stuff.
-- TODO: Check King is not in check after move (takes care of both problems - can't move into check and can't leave King in check either)
data MovePiece :: Piece -> Position -> Board -> Exp (Maybe Board)
type instance Eval (MovePiece piece toPos board) = Eval (Eval (MovePieceSwitch piece toPos board) >>= CheckNoCheck (Eval (PieceTeam piece)))

data CheckNoCheck :: Team -> Board -> Exp (Maybe Board)
type instance Eval (CheckNoCheck team board) = Eval (If (Eval (IsKingInCheck team board)) (ID Nothing) (ID $ Just board))

data MovePieceSwitch :: Piece -> Position -> Board -> Exp (Maybe Board)
type instance Eval (MovePieceSwitch piece toPos board) = Just $ Eval (Switch '[
    '(Eval (IsQueen piece), MovePieceTo piece toPos board),
    '(Eval (IsRook piece), MovePieceTo piece toPos board),
    '(Eval (IsBishop piece), MovePieceTo piece toPos board),
    '(Eval (IsKnight piece), MovePieceTo piece toPos board),
    '(Eval (IsKing piece), MoveKing piece toPos board),
    '(Eval (IsPawn piece), MovePawn piece toPos board) ] )

-- A variant of SetPieceAt, which increments the number of moves a piece has done,
-- and sets that piece as the last piece having moved.
data MovePieceTo :: Piece -> Position -> Board -> Exp Board
type instance Eval (MovePieceTo piece toPos board) = Eval (SetLastPieceMoved toPos (Eval (SetPieceAt (Eval (IncrementMoves piece)) board toPos)))

-- TODO: Handle castling (which should not increment the rook's move counter)
-- Ensuring you don't move into check is handled by MovePiece
data MoveKing :: Piece -> Position -> Board -> Exp Board
type instance Eval (MoveKing king toPos board) = Eval (MovePieceTo king toPos board)

-- TODO: Allow players to choose what to promote their pawn to!
-- TODO: Handle en passant in "else" branch
data MovePawn :: Piece -> Position -> Board -> Exp Board
type instance Eval (MovePawn (MkPiece Black Pawn info) (At col row) board) = Eval (If (Eval (row :==: 1)) (MovePieceTo (MkPiece Black Queen info) (At col row) board) (MovePieceTo (MkPiece Black Pawn info) (At col row) board))
type instance Eval (MovePawn (MkPiece White Pawn info) (At col row) board) = Eval (If (Eval (row :==: 8)) (MovePieceTo (MkPiece White Queen info) (At col row) board) (MovePieceTo (MkPiece White Pawn info) (At col row) board))

-----------------------------------------------------------------------------------------------

data NatLength :: [a] -> Exp Nat
type instance Eval (NatLength '[]) = Z
type instance Eval (NatLength (x ': xs)) = S $ Eval (NatLength xs)

type LEmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type LEmptyBoard = LEmptyRow :-> LEmptyRow :-> LEmptyRow :-> LEmptyRow :-> LEmptyRow :-> LEmptyRow :-> LEmptyRow :<> LEmptyRow


type MyTestInfo = Info Z (At A Nat1) False
type MyTestBoard = Eval (SetPiecesAt '[ '(MkPiece Black King MyTestInfo, At A Nat1), '(MkPiece White King MyTestInfo, At H Nat8), '(MkPiece Black Queen MyTestInfo, At D Nat3), '(MkPiece White Queen MyTestInfo, At E Nat3)] LEmptyBoard)

-- Eval (LazyAnd (Eval ('[] :<= '[])) ('[] :<= '[]))  -- Fails because of ambiguous types??
