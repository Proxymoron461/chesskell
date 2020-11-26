module Lib where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Vec
import ChessTypes
import Ranges
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
type instance Eval (GetAdjacentColumns col) = col ': Eval (Map FromJust (Eval (Filter IsJust '[Eval (Nat1 :+ col), Eval (Nat1 :- col)])))

data HasRow :: Nat -> Position -> Exp Bool
type instance Eval (HasRow x (At _ y)) = Eval ((x <=? y) :&&: ID (y <=? x))

-- Type families for getting all available squares in a straight line, with nothing in the way
data AllReachableFunc :: Team -> BoardDecorator -> Position -> (Position -> Exp [Position]) -> Exp [Position]
type instance Eval (AllReachableFunc team boardDec pos f) = Eval (TakeWhilePlus (Not . (IsPieceAt boardDec)) ((MaybeIf (Not . (HasTeam team))) . (GetPieceAtDec boardDec)) (Eval (f pos)))

data AllReachableLeft :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableLeft team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllLeft)

data AllReachableRight :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableRight team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllRight)

data AllReachableAbove :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableAbove team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllAbove)

data AllReachableBelow :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableBelow team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllBelow)

data AllReachableStraightLine :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableStraightLine team boardDec pos) = Eval (Concat (Eval (Map (AllReachableFunc team boardDec pos) '[ GetAllLeft, GetAllRight, GetAllAbove, GetAllBelow ])))

data AllReachableLineAndDiag :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableLineAndDiag team boardDec pos) = (Eval (AllReachableStraightLine team boardDec pos)) ++ (Eval (AllReachableDiag team boardDec pos))

-- Reachable square type families for all diagonal directions at once: helpful
-- for Bishops and Queens!
data AllReachableDiag :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableDiag team boardDec pos) = Eval (Concat (Eval (Map (AllReachableFunc team boardDec pos) '[ GetAllDiagNW, GetAllDiagSW, GetAllDiagSE, GetAllDiagNE ])))

-- Reachable square type families for each diagonal direction
data AllReachableDiagNW :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNW team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagNW)

data AllReachableDiagSW :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSW team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagSW)

data AllReachableDiagSE :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSE team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagSE)

data AllReachableDiagNE :: Team -> BoardDecorator -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNE team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagNE)

-- Prunes a list for all spaces taken up by a piece of the same team
-- (Perfect for kinds and knights!)
data AllReachableGivenList :: Team -> BoardDecorator -> [Position] -> Exp [Position]
type instance Eval (AllReachableGivenList team boardDec list) = Eval (Filter (FromMaybe True (Not . HasTeam team) . GetPieceAtDec boardDec) list)

-- General function, for taking the first N reachable positions from a particular direction.
-- NOTE: Relies on each directional function giving them in order of distance from the player
-- NOTE: Does not work with AllReachableDiag, as that will only be in one direction.
data NReachableFunc :: Team -> BoardDecorator -> Position -> (Team -> BoardDecorator -> Position -> Exp [Position]) -> TL.Nat -> Exp [Position]
type instance Eval (NReachableFunc team boardDec pos f n) = Eval (Take n (Eval (f team boardDec pos)))

data NReachableDiagNW :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagNW team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableDiagNW n)

data NReachableDiagNE :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagNE team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableDiagNE n)

data NReachableDiagSW :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagSW team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableDiagSW n)

data NReachableDiagSE :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableDiagSE team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableDiagSE n)

data NReachableBelow :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableBelow team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableBelow n)

data NReachableAbove :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableAbove team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableAbove n)

data NReachableLeft :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableLeft team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableLeft n)

data NReachableRight :: Team -> BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (NReachableRight team boardDec pos n) = Eval (NReachableFunc team boardDec pos AllReachableRight n)

data GetOneLeft :: Position -> Exp (Maybe Position)
type instance Eval (GetOneLeft (At col row)) = Eval (Eval (Nat1 :- col) >>= ((CW Just) . ((Flip (CW2 At)) row)))

data GetOneRight :: Position -> Exp (Maybe Position)
type instance Eval (GetOneRight (At col row)) = Eval (Eval (Nat1 :+ col) >>= ((CW Just) . ((Flip (CW2 At)) row)))

-- The pawn is the only piece whose attack rules differ from its' movement rules;
-- so it requires a special case.
data PawnReachableAbove :: BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (PawnReachableAbove boardDec pos n) = Eval (GetFreePositions (Eval (NReachableAbove White boardDec pos n)) boardDec)

data PawnReachableBelow :: BoardDecorator -> Position -> TL.Nat -> Exp [Position]
type instance Eval (PawnReachableBelow boardDec pos n) = Eval (GetFreePositions (Eval (NReachableBelow Black boardDec pos n)) boardDec)

data IsPieceAt :: BoardDecorator -> Position -> Exp Bool
type instance Eval (IsPieceAt boardDec pos) = Eval (IsJust (Eval (GetPieceAtDec boardDec pos)))

data IsKingAt :: Team -> BoardDecorator -> Position -> Exp Bool
type instance Eval (IsKingAt team boardDec pos) = Eval (FromMaybe False (HasTeam team .&. IsKing) (Eval (GetPieceAtDec boardDec pos)))

data IsRookAt :: Team -> BoardDecorator -> Position -> Exp Bool
type instance Eval (IsRookAt team boardDec pos) = Eval (FromMaybe False (HasTeam team .&. IsRook) (Eval (GetPieceAtDec boardDec pos)))

data IsQueenAt :: BoardDecorator -> Position -> Exp Bool
type instance Eval (IsQueenAt boardDec pos) = Eval (FromMaybe False IsQueen (Eval (GetPieceAtDec boardDec pos)))

data GetFreePositions :: [Position] -> BoardDecorator -> Exp [Position]
type instance Eval (GetFreePositions '[] _) = '[]
type instance Eval (GetFreePositions (p ': ps) boardDec) = Eval (If (Eval ((Eval (IsPieceAt boardDec p)) :||: ((Not . IsValidPosition) p))) (GetFreePositions ps boardDec) (ID (p ': (Eval (GetFreePositions ps boardDec)))))

-- :k Foldr :: (Row -> [Pos] -> Exp [Pos]) -> [Pos] -> [Row] -> Exp [Pos]
data GetUnderAttackPositions :: Team -> BoardDecorator -> Exp [Position]
type instance Eval (GetUnderAttackPositions team boardDec) = Eval (Foldr (AddRowMovesToList team boardDec) '[] (GetBoard boardDec))

data AddRowMovesToList :: Team -> BoardDecorator -> Row -> [Position] -> Exp [Position]
type instance Eval (AddRowMovesToList team boardDec row list) = Eval (GetRowUnderAttackPositions team boardDec row) ++ list

-- :k Foldr :: (Maybe Piece -> [Pos] -> Exp [Pos]) -> [Pos] -> [Maybe Piece] -> Exp [Pos]
data GetRowUnderAttackPositions :: Team -> BoardDecorator -> Row -> Exp [Position]
type instance Eval (GetRowUnderAttackPositions team boardDec row) = Eval (Foldr (AddMovesToList team boardDec) '[] row)

data AddMovesToList :: Team -> BoardDecorator -> Maybe Piece -> [Position] -> Exp [Position]
type instance Eval (AddMovesToList team boardDec maybePiece list) = Eval (FromMaybe '[] ((Flip PieceAttackList) boardDec) (Eval (If (Eval (MaybeIf (HasTeam team) maybePiece)) (ID maybePiece) (ID Nothing)))) ++ list

type family NoKingError (b :: BoardDecorator) (team :: Team) where
    NoKingError boardDec team = TL.TypeError ((TL.Text ("There is no " ++ TypeShow team ++ " King on the board!")) TL.:$$: (TL.Text ("Error at move: " ++ TypeShow (GetNoOfMoves boardDec))))

data FindKing :: Team -> BoardDecorator -> Exp Piece
type instance Eval (FindKing team boardDec) = Eval (FromMaybe (NoKingError boardDec team) ID (Eval (GetPieceAtDec boardDec (GetKingPosition team boardDec))))

-- TODO: Remove "Team" from input, since you can get it from board decorator??
data IsKingInCheck :: Team -> BoardDecorator -> Exp Bool
type instance Eval (IsKingInCheck team boardDec) = Eval (Any (IsKingAt team boardDec) (Eval (GetUnderAttackPositions (OppositeTeam' team) boardDec)))

-- This function just checks the spots a piece can move to; it does not handle moving itself.
-- That is in the other function, named Move.
-- Returns an empty list if the boardDec is empty at that position!
-- NOTE: This allows pieces to state that they can move to the King's position; but this is just for check purposes. They can't actually take the king.
-- TODO: Check that the piece's reported position is its' actual position
-- TODO: Test all this!!! Near-urgently!
data PieceMoveList :: Piece -> BoardDecorator -> Exp [Position]
type instance Eval (PieceMoveList (MkPiece team Pawn info) boardDec)   = Eval (If (Eval ((IsZero . GetMoveCount) info)) (PawnStartMove (MkPiece team Pawn info) boardDec) (PawnPostStart (MkPiece team Pawn info) boardDec))
type instance Eval (PieceMoveList (MkPiece team Bishop info) boardDec) = Eval (AllReachableDiag team boardDec (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Knight info) boardDec) = Eval (AllReachableGivenList team boardDec (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceMoveList (MkPiece team Rook info) boardDec)   = Eval (AllReachableStraightLine team boardDec (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Queen info) boardDec)  = Eval (AllReachableLineAndDiag team boardDec (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team King info) boardDec)   = KingMoveList (MkPiece team King info) boardDec

data PieceAttackList :: Piece -> BoardDecorator -> Exp [Position]
type instance Eval (PieceAttackList (MkPiece team Pawn info) boardDec)   = Eval (PawnTakePositions False (MkPiece team Pawn info) boardDec)
type instance Eval (PieceAttackList (MkPiece team Bishop info) boardDec) = Eval (AllReachableDiag team boardDec (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team Knight info) boardDec) = Eval (AllReachableGivenList team boardDec (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceAttackList (MkPiece team Rook info) boardDec)   = Eval (AllReachableStraightLine team boardDec (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team Queen info) boardDec)  = Eval (AllReachableLineAndDiag team boardDec (Eval (GetPosition info)))
type instance Eval (PieceAttackList (MkPiece team King info) boardDec)   = Eval (AllReachableGivenList team boardDec (Eval (GetAdjacent (Eval (GetPosition info)))))

-- Adds the castling positions to the King move list if applicable
type family KingMoveList (p :: Piece) (b :: BoardDecorator) :: [Position] where
    KingMoveList (MkPiece team King info) boardDec
        = (Eval (AllReachableGivenList team boardDec (Eval (GetAdjacent (Eval (GetPosition info)))))
            ++ GetCastlePositions team boardDec) 

-- data PieceHasMoveCount :: Nat -> Piece -> Exp Bool
type family CanCastle (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    CanCastle team boardDec = Eval (If (Not' (HasKingMoved team boardDec))
        (ID (CanCastleToEitherRook team boardDec))
        (ID '(False, False)))

type family CanCastleToEitherRook (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    CanCastleToEitherRook team boardDec = (Eval (PairAnd (HaveRooksNotMoved team boardDec)
        (Eval (PairAnd
            (Eval (PairPredicate (Eval (CastleSpacesToTest team boardDec)) (Not . AnySpaceInCheck team boardDec)))
            (Eval (PairPredicate (BetweenKingAndRook team) (AllSpacesFree boardDec)))))))

-- TODO: Replace with RangeBetweenExclusive or something like that??
type family BetweenKingAndRook (t :: Team) :: ([Position], [Position]) where
    BetweenKingAndRook White = '( SpacesBetweenInc (At D Nat1) (At B Nat1), '[ (At F Nat1), (At G Nat1) ])
    BetweenKingAndRook Black = '( SpacesBetweenInc (At D Nat8) (At B Nat8), '[ (At F Nat8), (At G Nat8) ])

type family HasKingMoved (t :: Team) (b :: BoardDecorator) :: Bool where
    HasKingMoved team boardDec = Eval (IsPieceAtWhichDec boardDec (GetKingPosition team boardDec) (Not . PieceHasMoveCount Z))

data PairAnd :: (Bool, Bool) -> (Bool, Bool) -> Exp (Bool, Bool)
type instance Eval (PairAnd '(l1, r1) '(l2, r2)) = '( Eval (l1 :&&: ID l2), Eval (r1 :&&: ID r2) )

type family Fst' (x :: (a, b)) :: a where
    Fst' '(x, _) = x

type family Snd' (x :: (a, b)) :: b where
    Snd' '(_, y) = y

type family Both' (x :: (Bool, Bool)) :: Bool where
    Both' '(True, True) = True
    Both' _             = False

type family Either' (x :: (Bool, Bool)) :: Bool where
    Either' '(False, False) = False
    Either' _               = True

data BothPredicate :: (a, b) -> (a -> Exp Bool) -> Exp Bool
type instance Eval (BothPredicate '( x, y ) f) = Eval (Eval (f x) :&&: f y)

data PairPredicate :: (a, b) -> (a -> Exp Bool) -> Exp (Bool, Bool)
type instance Eval (PairPredicate '( x, y ) f) = '( Eval (f x), Eval (f y))

type family HaveRooksNotMoved (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    HaveRooksNotMoved team boardDec = PairNot' (HaveRooksMoved team boardDec)

type family HaveRooksMoved (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    HaveRooksMoved team boardDec = HaveRooksMovedHelper (RookStartPositions team) boardDec

type family HaveRooksMovedHelper (r :: (Position, Position)) (b :: BoardDecorator) :: (Bool, Bool) where
    HaveRooksMovedHelper '( left, right ) boardDec
        = '( Eval ((Not . IsPieceAtWhichDec boardDec left) (PieceHasMoveCount Z)), Eval ((Not . IsPieceAtWhichDec boardDec right) (PieceHasMoveCount Z)) )

-- data GetUnderAttackPositions :: Team -> BoardDecorator -> Exp [Position]
-- Checks if any of a particular list of spaces is under attack
data AnySpaceInCheck :: Team -> BoardDecorator -> [Position] -> Exp Bool
type instance Eval (AnySpaceInCheck team boardDec xs) = Eval (Any ((Flip In) (Eval (GetUnderAttackPositions (OppositeTeam' team) boardDec))) xs)

data AllSpacesFree :: BoardDecorator -> [Position] -> Exp Bool
type instance Eval (AllSpacesFree boardDec xs) = Eval (All (Not . IsPieceAt boardDec) xs)

type family RookStartPositions (t :: Team) :: (Position, Position) where
    RookStartPositions White = '( At A Nat1, At H Nat1 )
    RookStartPositions Black = '( At A Nat8, At H Nat8 )

type family GetCastlePositions (t :: Team) (b :: BoardDecorator) :: [Position] where
    GetCastlePositions team boardDec = GetCastlePositionsHelper (CanCastle team boardDec) (GetKingPosition team boardDec)

type family GetCastlePositionsHelper (x :: (Bool, Bool)) (p :: Position) :: [Position] where
    GetCastlePositionsHelper '(False, False) kingPos = '[]
    GetCastlePositionsHelper '(True,  False) kingPos = '[ TwoLeft kingPos ]
    GetCastlePositionsHelper '(False, True)  kingPos = '[ TwoRight kingPos ]
    GetCastlePositionsHelper '(True,  True)  kingPos = '[ TwoLeft kingPos, TwoRight kingPos ]

data CastleSpacesToTest :: Team -> BoardDecorator -> Exp ([Position], [Position])
type instance Eval (CastleSpacesToTest team boardDec)
    = Eval (CastleSpacesToTestHelper (GetKingPosition team boardDec))

data CastleSpacesToTestHelper :: Position -> Exp ([Position], [Position])
type instance Eval (CastleSpacesToTestHelper pos)
    = '(SpacesBetweenInc pos (TwoLeft pos), SpacesBetweenInc pos (TwoRight pos))

-- :kind! Eval (((Flip (CW2 At)) Z) A) = At A Z
type family SpacesBetween (x :: Position) (y :: Position) :: [Position] where
    SpacesBetween (At col row1) (At col row2) = Eval (CW (At col) <$> Eval (RangeBetweenDTNat row1 row2))
    SpacesBetween (At col1 row) (At col2 row) = Eval (((Flip (CW2 At)) row) <$> Eval (ColRangeBetween col1 col2))

type family SpacesBetweenInc (x :: Position) (y :: Position) :: [Position] where
    SpacesBetweenInc pos1 pos2 = pos1 ': SpacesBetween pos1 pos2

-- First boolean argument determines the reach - the King check occurs if it is true,
-- and it does not if it is False.
data PieceCanReachKingCheck :: Bool -> Piece -> Position -> BoardDecorator -> Exp Bool
type instance Eval (PieceCanReachKingCheck True piece pos boardDec) = Eval (Eval (pos `In` Eval (PieceMoveList piece boardDec)) :&&: ((Not . IsKingAt (Eval (OppositeTeam (Eval (PieceTeam piece)))) boardDec) pos))
type instance Eval (PieceCanReachKingCheck False piece pos boardDec) = Eval (pos `In` Eval (PieceMoveList piece boardDec))

data PieceCanMoveTo :: Piece -> Position -> BoardDecorator -> Exp Bool
type instance Eval (PieceCanMoveTo piece pos boardDec) = Eval (PieceCanReachKingCheck True piece pos boardDec)

data CanMoveTo :: Position -> Position -> BoardDecorator -> Exp Bool
type instance Eval (CanMoveTo fromPos toPos boardDec) = Eval (FromMaybe False ((FlipToLast PieceCanMoveTo) toPos boardDec) (Eval (GetPieceAtDec boardDec fromPos)))

data PieceCanReach :: Piece -> Position -> BoardDecorator -> Exp Bool
type instance Eval (PieceCanReach piece pos boardDec) = Eval (PieceCanReachKingCheck False piece pos boardDec)

data CanReach :: Position -> Position -> BoardDecorator -> Exp Bool
type instance Eval (CanReach fromPos toPos boardDec) = Eval (FromMaybe False ((FlipToLast PieceCanReach) toPos boardDec) (Eval (GetPieceAtDec boardDec fromPos)))

-- Type family for where a pawn can move when it is in its' starting position
-- TODO: Throw a type error if the Pawn has already moved??
data PawnStartMove :: Piece -> BoardDecorator -> Exp [Position]
type instance Eval (PawnStartMove pawn boardDec) = (Eval (PawnMove pawn boardDec 2)) ++ (Eval (PawnTakePositions True pawn boardDec))

-- Type family for getting the initial pawn two-forward move!
data PawnMove :: Piece -> BoardDecorator -> TL.Nat -> Exp [Position]
type instance Eval (PawnMove (MkPiece Black Pawn info) boardDec n) = Eval (PawnReachableBelow boardDec (Eval (GetPosition info)) n)
type instance Eval (PawnMove (MkPiece White Pawn info) boardDec n) = Eval (PawnReachableAbove boardDec (Eval (GetPosition info)) n)

-- TODO: Check that the opposing piece is not a king??
type family PawnReachableDiagNE (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: [Position] where
    PawnReachableDiagNE _ _ (At H _) boardDec = '[]
    PawnReachableDiagNE _ _ (At _ Nat8) boardDec = '[]
    PawnReachableDiagNE False team (At col row) boardDec = '[ OneRight (At col (S row)) ]
    PawnReachableDiagNE True team (At col row) boardDec =
        Eval (If (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam (OppositeTeam' team))))
            (ID '[ OneRight (At col (S row)) ])
            (ID '[]))

type family PawnReachableDiagNW (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: [Position] where
    PawnReachableDiagNW _ _ (At A _) boardDec = '[]
    PawnReachableDiagNW _ _ (At _ Nat8) boardDec = '[]
    PawnReachableDiagNW False team (At col row) boardDec = '[ OneLeft (At col (S row)) ]
    PawnReachableDiagNW True team (At col row) boardDec =
        Eval (If (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam (OppositeTeam' team))))
            (ID '[ OneLeft (At col (S row)) ] )
            (ID '[]))

type family PawnReachableDiagSE (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: [Position] where
    PawnReachableDiagSE _ _ (At H _) boardDec = '[]
    PawnReachableDiagSE _ _ (At _ Z) boardDec = '[]
    PawnReachableDiagSE False team (At col (S row)) boardDec = '[ OneRight (At col row) ]
    PawnReachableDiagSE True team (At col (S row)) boardDec =
        Eval (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam (OppositeTeam' team))))
            (ID '[ OneRight (At col row) ])
            (ID '[]))

type family PawnReachableDiagSW (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: [Position] where
    PawnReachableDiagSW _ _ (At A _) boardDec = '[]
    PawnReachableDiagSW _ _ (At _ Z) boardDec = '[]
    PawnReachableDiagSW False team (At col (S row)) boardDec = '[ OneLeft (At col row) ]
    PawnReachableDiagSW True team (At col (S row)) boardDec =
        Eval (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam (OppositeTeam' team))))
            (ID '[ OneLeft (At col row) ])
            (ID '[]))

-- Pawns can take diagonally in front of themselves: so this gets those positions if a take is possible!
-- Boolean argument is for if the pawn should perform the IsPieceAt check.
-- TODO: Replace (++) with something a lil more efficient??
data PawnTakePositions :: Bool -> Piece -> BoardDecorator -> Exp [Position]
type instance Eval (PawnTakePositions doIsPieceCheck (MkPiece Black Pawn info) boardDec) = (PawnReachableDiagSE doIsPieceCheck Black (Eval (GetPosition info)) boardDec)
    ++ (PawnReachableDiagSW doIsPieceCheck Black (Eval (GetPosition info)) boardDec)
    ++ (Eval (GetEnPassantPosition (Eval (GetPosition info)) boardDec))
type instance Eval (PawnTakePositions doIsPieceCheck (MkPiece White Pawn info) boardDec) = (PawnReachableDiagNE doIsPieceCheck White (Eval (GetPosition info)) boardDec)
    ++ (PawnReachableDiagNW doIsPieceCheck White (Eval (GetPosition info)) boardDec)
    ++ (Eval (GetEnPassantPosition (Eval (GetPosition info)) boardDec))

-- TODO: Make it Position, not [Position] - only one space at a time is vulnerable to en passant!
-- Given a position and a board decorator, output a list of positions that can be moved to
-- which would perform en passant takes.
data GetEnPassantPosition :: Position -> BoardDecorator -> Exp [Position]
type instance Eval (GetEnPassantPosition pos boardDec) =
    Eval (If (Eval ((GetLastPosition boardDec) `In` Eval (GetLeftRightPositions pos)))
    (FromMaybe '[] (EnPassantPosition (GetMovingTeam boardDec) . PiecePosition)
        (Eval (GetPieceAtWhichDec boardDec (GetLastPosition boardDec) (IsPawn .&. PawnMovedTwoLast))))  -- then
    (ID '[]))  --else

data EnPassantPosition :: Team -> Position -> Exp [Position]
type instance Eval (EnPassantPosition team pos) = EnPassantPositionNonFCF team pos

type family EnPassantPositionNonFCF (t :: Team) (p :: Position) :: [Position] where
    EnPassantPositionNonFCF White (At col row)     = '[ At col (S row) ]
    EnPassantPositionNonFCF Black (At col (S row)) = '[ At col row ]

data ListReturn :: a -> Exp [a]
type instance Eval (ListReturn x) = '[ x ]

-- TODO: Non-FCF type family version??
data GetLeftRightPositions :: Position -> Exp [Position]
type instance Eval (GetLeftRightPositions pos) = Eval (FilterMap IsJust FromJust (Eval ('[GetOneLeft, GetOneRight] <*> '[ pos ])))

data PawnMovedTwoLast :: Piece -> Exp Bool
type instance Eval (PawnMovedTwoLast (MkPiece White Pawn info)) = Eval (((HasRow Nat4 . GetPosition) .&. (Equal Nat1 . GetMoveCount)) info)
type instance Eval (PawnMovedTwoLast (MkPiece Black Pawn info)) = Eval (((HasRow Nat5 . GetPosition) .&. (Equal Nat1 . GetMoveCount)) info)

data PawnPostStart :: Piece -> BoardDecorator -> Exp [Position]
type instance Eval (PawnPostStart pawn boardDec) = (Eval (PawnMove pawn boardDec 1)) ++ (Eval (PawnTakePositions True pawn boardDec))

-- Only moves a piece if it is of the correct type
-- Checks if the piece should have undergone a promotion
data IfPieceThenMove :: PieceName -> Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (IfPieceThenMove name fromPos toPos boardDec)
    = Eval (If (Eval (IsPieceAtWhichDec boardDec fromPos (IsPiece name)))
        ((ShouldHavePromotedCheck toPos . Move fromPos toPos) boardDec)
        (If (Eval (IsPieceAt boardDec fromPos))
            (TE' (TL.Text ("The piece at: " ++ TypeShow fromPos ++ " is not a " ++ TypeShow name ++ ".")))
            (TE' (TL.Text ("There is no piece at: " ++ TypeShow fromPos ++ ".")))))

data PromotePawnMove :: Position -> Position -> PieceName -> BoardDecorator -> Exp BoardDecorator
type instance Eval (PromotePawnMove fromPos toPos promoteTo boardDec)
    = Eval (If (Eval (IsPieceAtWhichDec boardDec fromPos (IsPiece Pawn)))
        ((PromotePieceTo promoteTo toPos . Move fromPos toPos) boardDec)
        (If (Eval (IsPieceAt boardDec fromPos))
            (TE' (TL.Text ("The piece at: " ++ TypeShow fromPos ++ " is not a " ++ TypeShow Pawn ++ ". Non-Pawn pieces cannot be promoted.")))
            (TE' (TL.Text ("There is no piece at: " ++ TypeShow fromPos ++ ".")))))

-- Type family for moving a piece, and putting it through a series of checks first
data Move :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (Move fromPos toPos boardDec) = Eval ((
    CheckNoCheck (GetMovingTeam boardDec) . MoveNoChecks fromPos toPos . 
        NotTakingKingCheck toPos .
        CanMoveCheck fromPos toPos .
        NotTakingOwnTeamCheck toPos .
        NotSamePosCheck fromPos toPos .
        NotLastToMoveCheck fromPos .
        TeamCheck fromPos) boardDec)

data MoveNoChecks :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
-- type instance Eval (MoveNoChecks fromPos toPos boardDec) = Eval (ClearPieceAtDec fromPos <$> (Eval (Eval (GetPieceAtDec boardDec fromPos) >>= (FlipToLast MovePiece) toPos boardDec)))
type instance Eval (MoveNoChecks fromPos toPos boardDec)
    = Eval (FromMaybeLazy (TE' (TL.Text "Something went wrong in: MoveNoChecks"))
        (ClearPieceAtDec fromPos . (FlipToLast MovePiece) toPos boardDec)
        (Eval (GetPieceAtDec boardDec fromPos)))

-- Does not check that it's valid the piece can move to the position, just moves them!
-- For all pieces apart from the King and the Pawn, moving them is easy: they move
-- a single piece, and have no side effects.
-- But the King can castle, and the Pawn can do en passant and turn into a Queen!
-- Very complicated stuff.
-- Checks King is not in check after move (takes care of 2 problems - can't move into check and can't leave King in check either)
data MovePiece :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePiece piece toPos boardDec) = Eval (MovePieceSwitch piece toPos boardDec)

-- Checks to be done BEFORE moving
data CanMoveCheck :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (CanMoveCheck fromPos toPos boardDec)
    = Eval (If (Eval (CanMoveTo fromPos toPos boardDec))
        (ID boardDec)
        (TE' ((TL.Text ("There is no valid move from " ++ TypeShow fromPos ++ " to " ++ TypeShow toPos ++ ".")
              TL.:$$: TL.Text ("The " ++ TypeShow (Eval ((PieceType . FromJust . GetPieceAtDec boardDec) fromPos)) ++ " at " ++ TypeShow fromPos ++ " can move to: " ++ TypeShow (Eval (PieceMoveList (FromJust' (Eval (GetPieceAtDec boardDec fromPos))) boardDec)))))))

data NotSamePosCheck :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotSamePosCheck fromPos toPos boardDec)
    = Eval (If (Eval (fromPos :==: toPos))
        (TE' (TL.Text ("Moves from a position to that same position are not allowed.")))
        (ID boardDec))

data NotTakingKingCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotTakingKingCheck toPos boardDec)
    = Eval (If (Eval (toPos `In` '[ GetKingPosition White boardDec, GetKingPosition Black boardDec ]))
        (TE' (TL.Text "A piece is not allowed to take a King. (Kings can only be beaten by putting them in checkmate.)"))
        (ID boardDec))

data NotTakingOwnTeamCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotTakingOwnTeamCheck toPos boardDec)
    = Eval (If (Eval (IsPieceAtWhichDec boardDec toPos (HasTeam (GetMovingTeam boardDec))))
        (TE' (TL.Text "A piece cannot take another piece on the same team!"))
        (ID boardDec))

data TeamCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (TeamCheck fromPos boardDec)
    = Eval (If (Eval (IsPieceAtWhichDec boardDec fromPos (HasTeam (GetMovingTeam boardDec))))
        (ID boardDec)
        (TE' (TL.Text ("The same team cannot move twice in a row."))))

data NotLastToMoveCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotLastToMoveCheck fromPos boardDec)
    = Eval (If (Eval (fromPos :==: (GetLastPosition boardDec)))
        (TE' (TL.Text ("The same piece is not allowed to move twice in a row.")))
        (ID boardDec))

-- Checks to be done AFTER moving
data CheckNoCheck :: Team -> BoardDecorator -> Exp BoardDecorator
type instance Eval (CheckNoCheck team boardDec) =
    Eval (If (Eval (IsKingInCheck team boardDec))
        (TE' (TL.Text ("The " ++ TypeShow team ++ " King is in check after a " ++ TypeShow team ++ " move. This is not allowed.")))
        (ID boardDec))

-- Checks if a promotion should have occurred
data ShouldHavePromotedCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (ShouldHavePromotedCheck toPos boardDec)
    = ShouldHavePromotedCheck' toPos boardDec

type family ShouldHavePromotedCheck' (t :: Position) (b :: BoardDecorator) :: BoardDecorator where
    ShouldHavePromotedCheck' (At col Nat8) boardDec
        = Eval (If (Eval (IsPieceAtWhichDec boardDec (At col Nat8) (IsPawn .&. HasTeam White)))
            (TE' (TL.Text ("Promotion should have occurred at: " ++ TypeShow (At col Nat8) ++ ". Pawns must be promoted when they reach the opposite end of the board.")))
            (ID boardDec))
    ShouldHavePromotedCheck' (At col Nat1) boardDec
        = Eval (If (Eval (IsPieceAtWhichDec boardDec (At col Nat1) (IsPawn .&. HasTeam Black)))
            (TE' (TL.Text ("Promotion should have occurred at: " ++ TypeShow (At col Nat1) ++ ". Pawns must be promoted when they reach the opposite end of the board.")))
            (ID boardDec))
    ShouldHavePromotedCheck' _ boardDec = boardDec

data DoChecks :: [ BoardDecorator -> Exp BoardDecorator ] -> BoardDecorator -> Exp BoardDecorator
type instance Eval (DoChecks '[]       boardDec) = boardDec
type instance Eval (DoChecks (f ': fs) boardDec) = Eval ((DoChecks fs . f) boardDec)

data MovePieceSwitch :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePieceSwitch piece toPos boardDec) = Eval (Switch '[
    '(Eval (IsQueen piece), MovePieceTo piece toPos boardDec),
    '(Eval (IsRook piece), MovePieceTo piece toPos boardDec),
    '(Eval (IsBishop piece), MovePieceTo piece toPos boardDec),
    '(Eval (IsKnight piece), MovePieceTo piece toPos boardDec),
    '(Eval (IsKing piece), MoveKing piece toPos boardDec),
    '(Eval (IsPawn piece), MovePawn piece toPos boardDec) ] )

-- A variant of SetPieceAtDec, which increments the number of moves a piece has done,
-- and sets that piece as the last piece having moved.
-- NOTE: This is the function that correctly sets the BoardDecorator.
data MovePieceTo :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePieceTo piece toPos (Dec board team pos kings moves))
    = Dec (Eval (SetPieceAt (Eval (IncrementMoves piece)) board toPos)) (Eval (PieceTeam piece)) toPos (UpdateKings kings piece toPos) (S moves)

-- Ensuring you don't move into check is handled by MovePiece
-- TODO: Flow through MovePieceTo
data MoveKing :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MoveKing king toPos boardDec) = Eval ( If (Eval (toPos `In` GetCastlePositions (Eval (PieceTeam king)) boardDec))
    ((CastleMoveRook toPos (RookStartPositions (GetMovingTeam boardDec)) . MovePieceTo king toPos) boardDec)
    (MovePieceTo king toPos boardDec))

data CastleMoveRook :: Position -> (Position, Position) -> BoardDecorator -> Exp BoardDecorator
type instance Eval (CastleMoveRook kingPos '(leftRook, rightRook) boardDec)
    = Eval (If (CloserToLeftRook kingPos)
        (SetPieceAtDecClear (Eval ((FromJust . GetPieceAtDec boardDec) leftRook)) boardDec (OneRight kingPos))
        (SetPieceAtDecClear (Eval ((FromJust . GetPieceAtDec boardDec) rightRook)) boardDec (OneLeft kingPos)))

type family CloserToLeftRook (x :: Position) :: Bool where
    CloserToLeftRook (At A _) = True
    CloserToLeftRook (At B _) = True
    CloserToLeftRook (At C _) = True
    CloserToLeftRook (At D _) = True
    CloserToLeftRook (At E _) = False
    CloserToLeftRook (At F _) = False
    CloserToLeftRook (At G _) = False
    CloserToLeftRook (At H _) = False

-- TODO: Flow through MovePieceTo
data MovePawn :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePawn (MkPiece team Pawn info) toPos boardDec) =
    Eval (If (Eval (toPos `In` (Eval (GetEnPassantPosition (GetPosition' info) boardDec))))
            ((EnPassantTakeAt team toPos . MovePieceTo (MkPiece team Pawn info) toPos) boardDec)
            (MovePieceTo (MkPiece team Pawn info) toPos boardDec))

data EnPassantTakeAt :: Team -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (EnPassantTakeAt White pos boardDec) = Eval (ClearPieceAtDec (OneDown pos) boardDec)
type instance Eval (EnPassantTakeAt Black pos boardDec) = Eval (ClearPieceAtDec (OneUp pos) boardDec)
