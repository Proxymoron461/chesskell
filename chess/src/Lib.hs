module Lib where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Vec
import ChessTypes
import Ranges
import Data.Type.Nat hiding (SNat(..))
import FingerTree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GetAllBelow :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllBelow pos) = GetAllBelow' pos

type family GetAllBelow' (n :: Position) :: FingerTree Position where
    GetAllBelow' (At col Nat1) = Empty
    GetAllBelow' (At col Nat2) = Single (At col Nat1)
    GetAllBelow' (At col Nat3) = Deep (One (At col Nat2)) Empty (One (At col Nat1))
    GetAllBelow' (At col Nat4) = Deep (Two (At col Nat3) (At col Nat2)) Empty (One (At col Nat1))
    GetAllBelow' (At col Nat5) = Deep (Three (At col Nat4) (At col Nat3) (At col Nat2)) Empty (One (At col Nat1))
    GetAllBelow' (At col Nat6) = Deep (Four (At col Nat5) (At col Nat4) (At col Nat3) (At col Nat2)) Empty (One (At col Nat1))
    GetAllBelow' (At col Nat7) = Deep (Four (At col Nat6) (At col Nat5) (At col Nat4) (At col Nat3)) Empty (Two (At col Nat2) (At col Nat1))
    GetAllBelow' (At col Nat8) = Deep (Four (At col Nat7) (At col Nat6) (At col Nat5) (At col Nat4)) Empty (Three (At col Nat3) (At col Nat2) (At col Nat1))

-- Takes in x and y, and performs x - y with a lower bound of 0
data SafeMinus :: TL.Nat -> TL.Nat -> Exp TL.Nat
type instance Eval (SafeMinus x y) = Eval (SafeMinusHelper x y (TL.CmpNat x y))

data SafeMinusHelper :: TL.Nat -> TL.Nat -> Ordering -> Exp TL.Nat
type instance Eval (SafeMinusHelper x y LT) = 0
type instance Eval (SafeMinusHelper x y EQ) = 0
type instance Eval (SafeMinusHelper x y GT) = x TL.- y

data GetAllAbove :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllAbove pos) = GetAllAbove' pos

type family GetAllAbove' (n :: Position) :: FingerTree Position where
    GetAllAbove' (At col Nat8) = Empty
    GetAllAbove' (At col Nat7) = Single (At col Nat8)
    GetAllAbove' (At col Nat6) = Deep (One (At col Nat7)) Empty (One (At col Nat8))
    GetAllAbove' (At col Nat5) = Deep (Two (At col Nat6) (At col Nat7)) Empty (One (At col Nat8))
    GetAllAbove' (At col Nat4) = Deep (Three (At col Nat5) (At col Nat6) (At col Nat7)) Empty (One (At col Nat8))
    GetAllAbove' (At col Nat3) = Deep (Four (At col Nat4) (At col Nat5) (At col Nat6) (At col Nat7)) Empty (One (At col Nat8))
    GetAllAbove' (At col Nat2) = Deep (Four (At col Nat3) (At col Nat4) (At col Nat5) (At col Nat6)) Empty (Two (At col Nat7) (At col Nat8))
    GetAllAbove' (At col Nat1) = Deep (Four (At col Nat2) (At col Nat3) (At col Nat4) (At col Nat5)) Empty (Three (At col Nat6) (At col Nat7) (At col Nat8))

data GetAllRight :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllRight pos) = GetAllRight' pos

-- TODO: Optimise with base cases!
type family GetAllRight' (n :: Position) :: FingerTree Position where
    GetAllRight' (At H row) = Empty
    GetAllRight' (At G row) = Single (At G row)
    GetAllRight' (At col row) = (At (R col) row) :< GetAllRight' (At (R col) row)

data GetAllLeft :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllLeft pos) = GetAllLeft' pos

-- TODO: Optimise with base cases!
type family GetAllLeft' (n :: Position) :: FingerTree Position where
    GetAllLeft' (At A row) = Empty
    GetAllLeft' (At B row) = Single (At A row)
    GetAllLeft' (At col row) = (At (L col) row) :< GetAllLeft' (At (L col) row)

data GetAllDiagNW :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllDiagNW pos) = GetAllDiagNW' pos

type family GetAllDiagNW' (p :: Position) :: FingerTree Position where
    GetAllDiagNW' (At col Nat8) = Single (At col Nat8)
    GetAllDiagNW' (At _ Z) = Empty
    GetAllDiagNW' (At _ Nat9) = Empty
    GetAllDiagNW' (At A row) = Single (At A row)
    GetAllDiagNW' (At B row) = At B row :< GetAllDiagNW' (At A (S row))
    GetAllDiagNW' (At C row) = At C row :< GetAllDiagNW' (At B (S row))
    GetAllDiagNW' (At D row) = At D row :< GetAllDiagNW' (At C (S row))
    GetAllDiagNW' (At E row) = At E row :< GetAllDiagNW' (At D (S row))
    GetAllDiagNW' (At F row) = At F row :< GetAllDiagNW' (At E (S row))
    GetAllDiagNW' (At G row) = At G row :< GetAllDiagNW' (At F (S row))
    GetAllDiagNW' (At H row) = At H row :< GetAllDiagNW' (At G (S row))

data GetAllDiagSW :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllDiagSW pos) = GetAllDiagSW' pos

type family GetAllDiagSW' (p :: Position) :: FingerTree Position where
    GetAllDiagSW' (At col Nat1) = Single (At col Nat1)
    GetAllDiagSW' (At _ Z) = Empty
    GetAllDiagSW' (At _ Nat9) = Empty
    GetAllDiagSW' (At A row) = Single (At A row)
    GetAllDiagSW' (At B (S row)) = (At B (S row)) :< GetAllDiagSW' (At A row)
    GetAllDiagSW' (At C (S row)) = (At C (S row)) :< GetAllDiagSW' (At B row)
    GetAllDiagSW' (At D (S row)) = (At D (S row)) :< GetAllDiagSW' (At C row)
    GetAllDiagSW' (At E (S row)) = (At E (S row)) :< GetAllDiagSW' (At D row)
    GetAllDiagSW' (At F (S row)) = (At F (S row)) :< GetAllDiagSW' (At E row)
    GetAllDiagSW' (At G (S row)) = (At G (S row)) :< GetAllDiagSW' (At F row)
    GetAllDiagSW' (At H (S row)) = (At H (S row)) :< GetAllDiagSW' (At G row)

data GetAllDiagSE :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllDiagSE pos) = GetAllDiagSE' pos

type family GetAllDiagSE' (p :: Position) :: FingerTree Position where
    GetAllDiagSE' (At col Nat1) = Single (At col Nat1)
    GetAllDiagSE' (At _ Z) = Empty
    GetAllDiagSE' (At _ Nat9) = Empty
    GetAllDiagSE' (At H row) = Single (At H row)
    GetAllDiagSE' (At A (S row)) = (At A (S row)) :< GetAllDiagSE' (At B row)
    GetAllDiagSE' (At B (S row)) = (At B (S row)) :< GetAllDiagSE' (At C row)
    GetAllDiagSE' (At C (S row)) = (At C (S row)) :< GetAllDiagSE' (At D row)
    GetAllDiagSE' (At D (S row)) = (At D (S row)) :< GetAllDiagSE' (At E row)
    GetAllDiagSE' (At E (S row)) = (At E (S row)) :< GetAllDiagSE' (At F row)
    GetAllDiagSE' (At F (S row)) = (At F (S row)) :< GetAllDiagSE' (At G row)
    GetAllDiagSE' (At G (S row)) = (At G (S row)) :< GetAllDiagSE' (At H row)

data GetAllDiagNE :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllDiagNE pos) = GetAllDiagNE' pos

type family GetAllDiagNE' (p :: Position) :: FingerTree Position where
    GetAllDiagNE' (At col Nat8) = Single (At col Nat8)
    GetAllDiagNE' (At _ Z) = Empty
    GetAllDiagNE' (At _ Nat9) = Empty
    GetAllDiagNE' (At H row) = Single (At H row)
    GetAllDiagNE' (At A row) = At A row :< GetAllDiagNE' (At B (S row))
    GetAllDiagNE' (At B row) = At B row :< GetAllDiagNE' (At C (S row))
    GetAllDiagNE' (At C row) = At C row :< GetAllDiagNE' (At D (S row))
    GetAllDiagNE' (At D row) = At D row :< GetAllDiagNE' (At E (S row))
    GetAllDiagNE' (At E row) = At E row :< GetAllDiagNE' (At F (S row))
    GetAllDiagNE' (At F row) = At F row :< GetAllDiagNE' (At G (S row))
    GetAllDiagNE' (At G row) = At G row :< GetAllDiagNE' (At H (S row))

data GetAllKnightPositions :: Position -> Exp (FingerTree Position)
type instance Eval (GetAllKnightPositions pos) = Eval (Filter IsValidPosition (Eval (GetKnightAboveBelow pos) ++ Eval (GetKnightLeftRight pos)))

-- TODO: Better implementation
data GetKnightAboveBelow :: Position -> Exp (FingerTree Position)
type instance Eval (GetKnightAboveBelow (At col row)) = Eval (FromList (Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 1)) <*> Eval (GetKnightRows row Nat2))))

-- TODO: Better implementation
data GetKnightLeftRight :: Position -> Exp (FingerTree Position)
type instance Eval (GetKnightLeftRight (At col row)) = Eval (FromList (Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 2)) <*> Eval (GetKnightRows row Nat1))))

data GetKnightColumns :: Column -> TL.Nat -> Exp [Column]
type instance Eval (GetKnightColumns col n) = Eval (GetKnightColumnsNat col (Eval (TLNatToNat n)))

data GetKnightColumnsNat :: Column -> Nat -> Exp [Column]
type instance Eval (GetKnightColumnsNat col n) = Eval (FilterMap (IsJust) (FromJust) '[ Eval (n :+ col), Eval (n :- col) ])

data GetKnightRows :: Nat -> Nat -> Exp [Nat]
type instance Eval (GetKnightRows row n) = If' (n <=? row) (ID '[ row - n, row + n]) (ID '[ row + n ])

-- :kind! Eval ((Eval ((CW FCFPlus) <$> [2,1,0])) <*> [1,2,3])
-- NOTE: Uses Tail to remove the current position!
data GetAdjacent :: Position -> Exp (FingerTree Position)
type instance Eval (GetAdjacent pos) = GetAdjacent' pos

type family GetAdjacent' (p :: Position) :: FingerTree Position where
    GetAdjacent' (At _ Z) = Empty
    GetAdjacent' (At A Nat1) = Empty :> At B Nat1 :> At B Nat2 :> At A Nat2
    GetAdjacent' (At A Nat8) = Empty :> At B Nat8 :> At B Nat7 :> At A Nat7
    GetAdjacent' (At A (S row)) = Empty :> At A (S (S row)) :> At B (S (S row)) :> At B (S row) :> At B row :> At A row 
    GetAdjacent' (At H Nat1) = Empty :> At G Nat1 :> At G Nat2 :> At H Nat2
    GetAdjacent' (At H Nat8) = Empty :> At G Nat8 :> At G Nat7 :> At H Nat7
    GetAdjacent' (At H (S row)) = Empty :> At H (S (S row)) :> At G (S (S row)) :> At G (S row) :> At G row :> At H row
    GetAdjacent' (At col Nat8) = Empty :> At (R col) Nat8 :> At (R col) Nat7 :> At col Nat7 :> At (L col) Nat7 :> At (L col) Nat8
    GetAdjacent' (At col Nat1) = Empty :> At (R col) Nat1 :> At (R col) Nat2 :> At col Nat2 :> At (L col) Nat2 :> At (L col) Nat1
    GetAdjacent' (At col (S row)) = Empty :> At (R col) (S row) :> At (R col) row :> At (R col) (S (S row)) :> At col row :> At col (S (S row)) :> At (L col) row :> At (L col) (S row) :> At (L col) (S (S row))

type family L (c :: Column) :: Column where
    L B = A
    L C = B
    L D = C
    L E = D
    L F = E
    L G = F
    L H = G
    L A = TL.TypeError (TL.Text "Cannot go left from column A.")

type family R (c :: Column) :: Column where
    R A = B
    R B = C
    R C = D
    R D = E
    R E = F
    R F = G
    R G = H
    R H = TL.TypeError (TL.Text "Cannot go right from column H.")

data HasRow :: Nat -> Position -> Exp Bool
type instance Eval (HasRow x (At _ y)) = Eval ((x <=? y) :&&: ID (y <=? x))

-- Type families for getting all available squares in a straight line, with nothing in the way
data AllReachableFunc :: Team -> BoardDecorator -> Position -> (Position -> Exp (FingerTree Position)) -> Exp (FingerTree Position)
type instance Eval (AllReachableFunc team boardDec pos f) = Eval (TakeWhilePlus (Not . (IsPieceAt boardDec)) ((MaybeIf (Not . (HasTeam team))) . (GetPieceAtDec boardDec)) (Eval (f pos)))

data AllReachableLeft :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableLeft team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllLeft)

data AllReachableRight :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableRight team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllRight)

data AllReachableAbove :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableAbove team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllAbove)

data AllReachableBelow :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableBelow team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllBelow)

data AllReachableStraightLine :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableStraightLine team boardDec pos)
    = Eval (AllReachableFunc team boardDec pos GetAllLeft)
      ++ Eval (AllReachableFunc team boardDec pos GetAllRight)
      ++ Eval (AllReachableFunc team boardDec pos GetAllAbove)
      ++ Eval (AllReachableFunc team boardDec pos GetAllBelow)

data AllReachableLineAndDiag :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableLineAndDiag team boardDec pos) = (Eval (AllReachableStraightLine team boardDec pos)) ++ (Eval (AllReachableDiag team boardDec pos))

-- Reachable square type families for all diagonal directions at once: helpful
-- for Bishops and Queens!
data AllReachableDiag :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableDiag team boardDec pos)
    = Eval (AllReachableFunc team boardDec pos GetAllDiagNW)
      ++ Eval (AllReachableFunc team boardDec pos GetAllDiagSW)
      ++ Eval (AllReachableFunc team boardDec pos GetAllDiagSE)
      ++ Eval (AllReachableFunc team boardDec pos GetAllDiagNE)

-- type family AllReachableDiag (t :: Team) (b :: BoardDecorator) (p :: Position) :: FingerTree Position where


-- Reachable square type families for each diagonal direction
data AllReachableDiagNW :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableDiagNW team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagNW)

data AllReachableDiagSW :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableDiagSW team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagSW)

data AllReachableDiagSE :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableDiagSE team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagSE)

data AllReachableDiagNE :: Team -> BoardDecorator -> Position -> Exp (FingerTree Position)
type instance Eval (AllReachableDiagNE team boardDec pos) = Eval (AllReachableFunc team boardDec pos GetAllDiagNE)

-- Prunes a list for all spaces taken up by a piece of the same team
-- (Perfect for kinds and knights!)
data AllReachableGivenList :: Team -> BoardDecorator -> FingerTree Position -> Exp (FingerTree Position)
type instance Eval (AllReachableGivenList team boardDec list) = Eval (Filter (FromMaybe True (Not . HasTeam team) . GetPieceAtDec boardDec) list)

-- The pawn is the only piece whose attack rules differ from its' movement rules;
-- so it requires a special case.
data PawnReachableAbove :: BoardDecorator -> Position -> TL.Nat -> Exp (FingerTree Position)
type instance Eval (PawnReachableAbove boardDec pos n) = Eval (GetFreePositions (PawnReachableAbove' pos n) boardDec)

type family PawnReachableAbove' (p :: Position) (n :: TL.Nat) :: FingerTree Position where
    PawnReachableAbove' (At col Nat8) 1 = Empty
    PawnReachableAbove' (At col row) 1 = Single (At col (S row))
    PawnReachableAbove' (At col Nat8) 2 = Empty
    PawnReachableAbove' (At col Nat7) 2 = Single (At col Nat8)
    PawnReachableAbove' (At col row) 2 = Deep (One (At col (S row))) Empty (One (At col (S (S row))))
    PawnReachableAbove' _ n = TL.TypeError (TL.Text "Pawn cannot move anything other than 1 or 2 spaces!")

data PawnReachableBelow :: BoardDecorator -> Position -> TL.Nat -> Exp (FingerTree Position)
type instance Eval (PawnReachableBelow boardDec pos n) = Eval (GetFreePositions (PawnReachableBelow' pos n) boardDec)

type family PawnReachableBelow' (p :: Position) (n :: TL.Nat) :: FingerTree Position where
    PawnReachableBelow' (At col Nat1) 1 = Empty
    PawnReachableBelow' (At col (S row)) 1 = Single (At col row)
    PawnReachableBelow' (At col Nat1) 2 = Empty
    PawnReachableBelow' (At col Nat2) 2 = Single (At col Nat1)
    PawnReachableBelow' (At col (S (S row))) 2 = Deep (One (At col (S row))) Empty (One (At col row))
    PawnReachableBelow' _ n = TL.TypeError (TL.Text "Pawn cannot move anything other than 1 or 2 spaces!")

data IsPieceAt :: BoardDecorator -> Position -> Exp Bool
type instance Eval (IsPieceAt boardDec pos) = Eval (IsJust (Eval (GetPieceAtDec boardDec pos)))

data IsKingAt :: Team -> BoardDecorator -> Position -> Exp Bool
type instance Eval (IsKingAt team boardDec pos) = Eval (FromMaybe False (HasTeam team .&. IsKing) (Eval (GetPieceAtDec boardDec pos)))

data IsRookAt :: Team -> BoardDecorator -> Position -> Exp Bool
type instance Eval (IsRookAt team boardDec pos) = Eval (FromMaybe False (HasTeam team .&. IsRook) (Eval (GetPieceAtDec boardDec pos)))

data IsQueenAt :: BoardDecorator -> Position -> Exp Bool
type instance Eval (IsQueenAt boardDec pos) = Eval (FromMaybe False IsQueen (Eval (GetPieceAtDec boardDec pos)))

data GetFreePositions :: FingerTree Position -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (GetFreePositions tree boardDec) = GetFreePositions' tree boardDec

type family GetFreePositions' (f :: FingerTree Position) (b :: BoardDecorator) :: FingerTree Position where
    GetFreePositions' Empty _ = Empty
    GetFreePositions' tree  boardDec
        = If' (Eval ((Eval (IsPieceAt boardDec (Head tree))) :||: ((Not . IsValidPosition) (Head tree))))
            (GetFreePositions (Tail' tree) boardDec)
            (ID ((Head tree) :< (Eval (GetFreePositions (Tail' tree) boardDec))))

-- :k Foldr :: (Row -> [Pos] -> Exp [Pos]) -> [Pos] -> [Row] -> Exp [Pos]
data GetUnderAttackPositions :: Team -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (GetUnderAttackPositions team boardDec) = Eval (Foldr (AddRowMovesToList team boardDec) Empty (GetBoard boardDec))

data AddRowMovesToList :: Team -> BoardDecorator -> Row -> FingerTree Position -> Exp (FingerTree Position)
type instance Eval (AddRowMovesToList team boardDec row list) = Eval (Foldr (AddMovesToList team boardDec) list row)

data AddMovesToList :: Team -> BoardDecorator -> Maybe Piece -> FingerTree Position -> Exp (FingerTree Position)
type instance Eval (AddMovesToList team boardDec maybePiece list)
    = Eval (FromMaybe Empty ((Flip PieceAttackList) boardDec) (If' (Eval (MaybeIf (HasTeam team) maybePiece)) (ID maybePiece) (ID Nothing))) ++ list

type family NoKingError (b :: BoardDecorator) (team :: Team) where
    NoKingError boardDec team = TL.TypeError ((TL.Text ("There is no " ++ TypeShow team ++ " King on the board!")) TL.:$$: (TL.Text ("Error at move: " ++ TypeShow (GetNoOfMoves boardDec))))

data FindKing :: Team -> BoardDecorator -> Exp Piece
type instance Eval (FindKing team boardDec) = Eval (FromMaybe (NoKingError boardDec team) ID (Eval (GetPieceAtDec boardDec (GetKingPosition team boardDec))))

-- New version definitely faster - tested in REPL. But still has memory issues??
data IsKingInCheck :: Team -> BoardDecorator -> Exp Bool
-- type instance Eval (IsKingInCheck team boardDec) = Eval (Any (IsKingAt team boardDec) (Eval (GetUnderAttackPositions (OppositeTeam' team) boardDec)))
type instance Eval (IsKingInCheck team boardDec) = Eval (IsKingInCheckHelper (GetKingPosition team boardDec) team boardDec)

data IsKingInCheckHelper :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (IsKingInCheckHelper kingPos team boardDec) = Eval (LazyAny '[
    SendLeftRay kingPos team boardDec,  -- Just this on its own causes 20GB. What??
    SendRightRay kingPos team boardDec,
    SendAboveRay kingPos team boardDec,
    SendBelowRay kingPos team boardDec, 
    SendNWRay kingPos team boardDec,
    SendNERay kingPos team boardDec,
    SendSWRay kingPos team boardDec,
    SendSERay kingPos team boardDec,
    IsKnightAttacking kingPos team boardDec ])
-- type instance Eval (IsKingInCheckHelper kingPos team boardDec) = EagerAny '[
--     SendLeftRay' kingPos team boardDec,  -- Also 25GB. What the heck.
--     SendRightRay' kingPos team boardDec,
--     SendAboveRay' kingPos team boardDec,
--     SendBelowRay' kingPos team boardDec,
--     SendNWRay' kingPos team boardDec,
--     SendNERay' kingPos team boardDec,
--     SendSWRay' kingPos team boardDec,
--     SendSERay' kingPos team boardDec,
--     Eval (IsKnightAttacking kingPos team boardDec) ]

data LazyAny :: [Exp Bool] -> Exp Bool
type instance Eval (LazyAny '[]) = False
type instance Eval (LazyAny (x ': xs)) = Eval ((Eval x) :||: LazyAny xs)

type family EagerAny (x :: [Bool]) :: Bool where
    EagerAny '[] = False
    EagerAny (True ': _) = True
    EagerAny (False ': xs) = EagerAny xs

-- This function just checks the spots a piece can move to; it does not handle moving itself.
-- That is in the other function, named Move.
-- Returns an empty list if the boardDec is empty at that position!
-- NOTE: This allows pieces to state that they can move to the King's position; but this is just for check purposes. They can't actually take the king.
-- TODO: Check that the piece's reported position is its' actual position
-- TODO: Test all this!!! Near-urgently!
data PieceMoveList :: Piece -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (PieceMoveList (MkPiece team Pawn info) boardDec)   = If' (Eval ((IsZero . GetMoveCount) info)) (PawnStartMove (MkPiece team Pawn info) boardDec) (PawnPostStart (MkPiece team Pawn info) boardDec)
type instance Eval (PieceMoveList (MkPiece team Bishop info) boardDec) = Eval (AllReachableDiag team boardDec (GetPosition' info))
type instance Eval (PieceMoveList (MkPiece team Knight info) boardDec) = Eval (AllReachableGivenList team boardDec (Eval (GetAllKnightPositions (GetPosition' info))))
type instance Eval (PieceMoveList (MkPiece team Rook info) boardDec)   = Eval (AllReachableStraightLine team boardDec (GetPosition' info))
type instance Eval (PieceMoveList (MkPiece team Queen info) boardDec)  = Eval (AllReachableLineAndDiag team boardDec (GetPosition' info))
type instance Eval (PieceMoveList (MkPiece team King info) boardDec)   = KingMoveList (MkPiece team King info) boardDec

data PieceAttackList :: Piece -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (PieceAttackList (MkPiece team Pawn info) boardDec)   = Eval (PawnTakePositions False (MkPiece team Pawn info) boardDec)
type instance Eval (PieceAttackList (MkPiece team Bishop info) boardDec) = Eval (AllReachableDiag team boardDec (GetPosition' info))
type instance Eval (PieceAttackList (MkPiece team Knight info) boardDec) = Eval (AllReachableGivenList team boardDec (Eval (GetAllKnightPositions (GetPosition' info))))
type instance Eval (PieceAttackList (MkPiece team Rook info) boardDec)   = Eval (AllReachableStraightLine team boardDec (GetPosition' info))
type instance Eval (PieceAttackList (MkPiece team Queen info) boardDec)  = Eval (AllReachableLineAndDiag team boardDec (GetPosition' info))
type instance Eval (PieceAttackList (MkPiece team King info) boardDec)   = Eval (AllReachableGivenList team boardDec (Eval (GetAdjacent (GetPosition' info))))

-- Adds the castling positions to the King move list if applicable
type family KingMoveList (p :: Piece) (b :: BoardDecorator) :: FingerTree Position where
    KingMoveList (MkPiece team King info) boardDec
        = (Eval (AllReachableGivenList team boardDec (Eval (GetAdjacent (GetPosition' info))))
            ++ GetCastlePositions team boardDec)

-- data PieceHasMoveCount :: Nat -> Piece -> Exp Bool
type family CanCastle (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    CanCastle team boardDec = If' (Not' (HasKingMoved team boardDec))
        (ID (CanCastleToEitherRook team boardDec))
        (ID '(False, False))

type family CanCastleToEitherRook (t :: Team) (b :: BoardDecorator) :: (Bool, Bool) where
    CanCastleToEitherRook team boardDec = (Eval (PairAnd (HaveRooksNotMoved team boardDec)
        (Eval (PairAnd
            (Eval (PairPredicate (Eval (CastleSpacesToTest team boardDec)) (Not . AnySpaceInCheck team boardDec)))
            (Eval (PairPredicate (BetweenKingAndRook team) (AllSpacesFree boardDec)))))))

-- TODO: Replace with RangeBetweenExclusive or something like that??
type family BetweenKingAndRook (t :: Team) :: (FingerTree Position, FingerTree Position) where
    BetweenKingAndRook White = '( Deep (Two (At D Nat1) (At C Nat1)) Empty (One (At B Nat1)), Deep (One (At F Nat1)) Empty (One (At G Nat1)) )
    BetweenKingAndRook Black = '( Deep (Two (At D Nat8) (At C Nat8)) Empty (One (At B Nat8)), Deep (One (At F Nat8)) Empty (One (At G Nat8)) )

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

-- data GetUnderAttackPositions :: Team -> BoardDecorator -> Exp (FingerTree Position)
-- Checks if any of a particular list of spaces is under attack
data AnySpaceInCheck :: Team -> BoardDecorator -> FingerTree Position -> Exp Bool
type instance Eval (AnySpaceInCheck team boardDec xs) = Eval (Any ((Flip In) (Eval (GetUnderAttackPositions (OppositeTeam' team) boardDec))) xs)

data AllSpacesFree :: BoardDecorator -> FingerTree Position -> Exp Bool
type instance Eval (AllSpacesFree boardDec xs) = Eval (All (Not . IsPieceAt boardDec) xs)

type family RookStartPositions (t :: Team) :: (Position, Position) where
    RookStartPositions White = '( At A Nat1, At H Nat1 )
    RookStartPositions Black = '( At A Nat8, At H Nat8 )

type family GetCastlePositions (t :: Team) (b :: BoardDecorator) :: FingerTree Position where
    GetCastlePositions team boardDec = GetCastlePositionsHelper (CanCastle team boardDec) (GetKingPosition team boardDec)

type family GetCastlePositionsHelper (x :: (Bool, Bool)) (p :: Position) :: FingerTree Position where
    GetCastlePositionsHelper '(False, False) kingPos = Empty
    GetCastlePositionsHelper '(True,  False) kingPos = Single (TwoLeft kingPos)
    GetCastlePositionsHelper '(False, True)  kingPos = Single (TwoRight kingPos)
    GetCastlePositionsHelper '(True,  True)  kingPos = Deep (One (TwoLeft kingPos)) Empty (One (TwoRight kingPos))

data CastleSpacesToTest :: Team -> BoardDecorator -> Exp (FingerTree Position, FingerTree Position)
type instance Eval (CastleSpacesToTest team boardDec)
    = CastleSpacesToTestHelper' (GetKingPosition team boardDec)

type family CastleSpacesToTestHelper' (p :: Position) where
    CastleSpacesToTestHelper' (At E row) = '( Deep (One (At D row)) Empty (One (At C row)), Deep (One (At F row)) Empty (One (At G row)) )
    CastleSpacesToTestHelper' _ = '( Empty, Empty )

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
data PawnStartMove :: Piece -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (PawnStartMove pawn boardDec) = (Eval (PawnMove pawn boardDec 2)) ++ (Eval (PawnTakePositions True pawn boardDec))

-- Type family for getting the initial pawn two-forward move!
data PawnMove :: Piece -> BoardDecorator -> TL.Nat -> Exp (FingerTree Position)
type instance Eval (PawnMove (MkPiece Black Pawn info) boardDec n) = Eval (PawnReachableBelow boardDec (GetPosition' info) n)
type instance Eval (PawnMove (MkPiece White Pawn info) boardDec n) = Eval (PawnReachableAbove boardDec (GetPosition' info) n)

-- TODO: Check that the opposing piece is not a king??
type family PawnReachableDiagNE (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: FingerTree Position where
    PawnReachableDiagNE _ _ (At H _) boardDec = Empty
    PawnReachableDiagNE _ _ (At _ Nat8) boardDec = Empty
    PawnReachableDiagNE False team (At col row) boardDec = Single (At (R col) (S row))
    PawnReachableDiagNE True team (At col row) boardDec =
        If' (Eval (IsPieceAtWhichDec boardDec (At (R col) (S row)) (HasTeam (OppositeTeam' team))))
            (ID (Single (At (R col) (S row))))
            (ID Empty)

type family PawnReachableDiagNW (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: FingerTree Position where
    PawnReachableDiagNW _ _ (At A _) boardDec = Empty
    PawnReachableDiagNW _ _ (At _ Nat8) boardDec = Empty
    PawnReachableDiagNW False team (At col row) boardDec = Single (At (L col) (S row))
    PawnReachableDiagNW True team (At col row) boardDec =
        If' (Eval (IsPieceAtWhichDec boardDec (At (L col) (S row)) (HasTeam (OppositeTeam' team))))
            (ID (Single (At (L col) (S row)) ))
            (ID Empty)

type family PawnReachableDiagSE (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: FingerTree Position where
    PawnReachableDiagSE _ _ (At H _) boardDec = Empty
    PawnReachableDiagSE _ _ (At _ Nat1) boardDec = Empty
    PawnReachableDiagSE False team (At col (S row)) boardDec = Single (At (R col) row)
    PawnReachableDiagSE True team (At col (S row)) boardDec =
        If' (Eval (IsPieceAtWhichDec boardDec (At (R col) row) (HasTeam (OppositeTeam' team))))
            (ID (Single (At (R col) row)))
            (ID Empty)

type family PawnReachableDiagSW (c :: Bool) (t :: Team) (p :: Position) (b :: BoardDecorator) :: FingerTree Position where
    PawnReachableDiagSW _ _ (At A _) boardDec = Empty
    PawnReachableDiagSW _ _ (At _ Nat1) boardDec = Empty
    PawnReachableDiagSW False team (At col (S row)) boardDec = Single (At (L col) row)
    PawnReachableDiagSW True team (At col (S row)) boardDec =
        If' (Eval (IsPieceAtWhichDec boardDec (At (L col) row) (HasTeam (OppositeTeam' team))))
            (ID (Single (At (L col) row)))
            (ID Empty)

-- Pawns can take diagonally in front of themselves: so this gets those positions if a take is possible!
-- Boolean argument is for if the pawn should perform the IsPieceAt check.
-- TODO: Replace (++) with something a lil more efficient??
data PawnTakePositions :: Bool -> Piece -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (PawnTakePositions doIsPieceCheck (MkPiece Black Pawn info) boardDec) = (PawnReachableDiagSE doIsPieceCheck Black (GetPosition' info) boardDec)
    ++ (PawnReachableDiagSW doIsPieceCheck Black (GetPosition' info) boardDec)
    ++ (Eval (GetEnPassantPosition (GetPosition' info) boardDec))
type instance Eval (PawnTakePositions doIsPieceCheck (MkPiece White Pawn info) boardDec) = (PawnReachableDiagNE doIsPieceCheck White (GetPosition' info) boardDec)
    ++ (PawnReachableDiagNW doIsPieceCheck White (GetPosition' info) boardDec)
    ++ (Eval (GetEnPassantPosition (GetPosition' info) boardDec))

-- TODO: Make it Position, not FingerTree Position - only one space at a time is vulnerable to en passant!
-- Given a position and a board decorator, output a list of positions that can be moved to
-- which would perform en passant takes.
data GetEnPassantPosition :: Position -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (GetEnPassantPosition pos boardDec) =
    If' (Eval ((GetLastPosition boardDec) `In` Eval (GetLeftRightPositions pos)))
    (FromMaybe Empty (EnPassantPosition (GetMovingTeam boardDec) . PiecePosition)
        (Eval (GetPieceAtWhichDec boardDec (GetLastPosition boardDec) (IsPawn .&. PawnMovedTwoLast))))  -- then
        (ID Empty)  --else

data EnPassantPosition :: Team -> Position -> Exp (FingerTree Position)
type instance Eval (EnPassantPosition team pos) = EnPassantPositionNonFCF team pos

type family EnPassantPositionNonFCF (t :: Team) (p :: Position) :: FingerTree Position where
    EnPassantPositionNonFCF White (At col row)     = Single (At col (S row))
    EnPassantPositionNonFCF Black (At col (S row)) = Single (At col row)

-- TODO: Non-FCF type family version??
data GetLeftRightPositions :: Position -> Exp (FingerTree Position)
type instance Eval (GetLeftRightPositions pos) = GetLeftRightPositions' pos

type family GetLeftRightPositions' (p :: Position) :: FingerTree Position where
    GetLeftRightPositions' (At A   row) = Single (At B row)
    GetLeftRightPositions' (At H   row) = Single (At G row)
    GetLeftRightPositions' (At col row) = Single (At (L col) row) :> At (R col) row

data PawnMovedTwoLast :: Piece -> Exp Bool
type instance Eval (PawnMovedTwoLast (MkPiece White Pawn info)) = Eval (((HasRow Nat4 . GetPosition) .&. (Equal Nat1 . GetMoveCount)) info)
type instance Eval (PawnMovedTwoLast (MkPiece Black Pawn info)) = Eval (((HasRow Nat5 . GetPosition) .&. (Equal Nat1 . GetMoveCount)) info)

data PawnPostStart :: Piece -> BoardDecorator -> Exp (FingerTree Position)
type instance Eval (PawnPostStart pawn boardDec) = (Eval (PawnMove pawn boardDec 1)) ++ (Eval (PawnTakePositions True pawn boardDec))

-- Only moves a piece if it is of the correct type
-- Checks if the piece should have undergone a promotion
data IfPieceThenMove :: PieceName -> Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (IfPieceThenMove name fromPos toPos boardDec)
    = If' (Eval (IsPieceAtWhichDec boardDec fromPos (IsPiece name)))
        (Move fromPos toPos boardDec)
        (If (Eval (IsPieceAt boardDec fromPos))
            (TE' (TL.Text ("The piece at: " ++ TypeShow fromPos ++ " is not a " ++ TypeShow name ++ ".")))
            (TE' (TL.Text ("There is no piece at: " ++ TypeShow fromPos ++ "."))))

data PromotePawnMove :: Position -> Position -> PieceName -> BoardDecorator -> Exp BoardDecorator
type instance Eval (PromotePawnMove fromPos toPos promoteTo boardDec)
    = If' (Eval (IsPieceAtWhichDec boardDec fromPos (IsPiece Pawn)))
        ((PromotePieceTo promoteTo toPos . Move fromPos toPos) boardDec)
        (If (Eval (IsPieceAt boardDec fromPos))
            (TE' (TL.Text ("The piece at: " ++ TypeShow fromPos ++ " is not a " ++ TypeShow Pawn ++ ". Non-Pawn pieces cannot be promoted.")))
            (TE' (TL.Text ("There is no piece at: " ++ TypeShow fromPos ++ "."))))

-- Type family for moving a piece, and putting it through a series of checks first
data Move :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (Move fromPos toPos boardDec) = Eval ((ShouldHavePromotedCheck toPos . CheckNoCheck) (Eval (MoveWithPreChecks fromPos toPos boardDec)))

data MoveWithPreChecks :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MoveWithPreChecks fromPos toPos boardDec) = Eval (
    (MoveNoChecks fromPos toPos .  -- 1m37 and 25GB!
        CanMoveCheck fromPos toPos .
        NotTakingKingCheck toPos .
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
    = If' (Eval (CanMoveTo fromPos toPos boardDec))
        (ID boardDec)
        (TE' ((TL.Text ("There is no valid move from " ++ TypeShow fromPos ++ " to " ++ TypeShow toPos ++ ".")
              TL.:$$: TL.Text ("The " ++ TypeShow (Eval ((PieceType . FromJust . GetPieceAtDec boardDec) fromPos)) ++ " at " ++ TypeShow fromPos ++ " can move to: " ++ TypeShow (Eval (PieceMoveList (FromJust' (Eval (GetPieceAtDec boardDec fromPos))) boardDec))))))

data NotSamePosCheck :: Position -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotSamePosCheck fromPos toPos boardDec)
    = If' (Eval (fromPos :==: toPos))
        (TE' (TL.Text ("Moves from a position to that same position are not allowed.")))
        (ID boardDec)

data NotTakingKingCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotTakingKingCheck toPos boardDec)
    = If' (Eval (toPos `In` '[ GetKingPosition White boardDec, GetKingPosition Black boardDec ]))
        (TE' (TL.Text "A piece is not allowed to take a King. (Kings can only be beaten by putting them in checkmate.)"))
        (ID boardDec)

data NotTakingOwnTeamCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotTakingOwnTeamCheck toPos boardDec)
    = If' (Eval (IsPieceAtWhichDec boardDec toPos (HasTeam (GetMovingTeam boardDec))))
        (TE' (TL.Text "A piece cannot take another piece on the same team!"))
        (ID boardDec)

data TeamCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (TeamCheck fromPos boardDec)
    = If' (Eval (IsPieceAtWhichDec boardDec fromPos (HasTeam (GetMovingTeam boardDec))))
        (ID boardDec)
        (TE' (TL.Text ("The same team cannot move twice in a row.")))

data NotLastToMoveCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (NotLastToMoveCheck fromPos boardDec)
    = If' (Eval (fromPos :==: (GetLastPosition boardDec)))
        (TE' (TL.Text ("The same piece is not allowed to move twice in a row.")))
        (ID boardDec)

-- Checks to be done AFTER moving
data CheckNoCheck :: BoardDecorator -> Exp BoardDecorator
type instance Eval (CheckNoCheck boardDec) =
    If' (Eval (IsKingInCheck (GetLastTeam boardDec) boardDec))
        (TE' (TL.Text ("The " ++ TypeShow (GetLastTeam boardDec) ++ " King is in check after a " ++ TypeShow (GetLastTeam boardDec) ++ " move. This is not allowed.")))
        (ID boardDec)
-- type instance Eval (CheckNoCheck boardDec) = boardDec

-- TODO: Check from King's position which pieces are reachable!

data SendLeftRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendLeftRay kingPos team boardDec) = SendLeftRay' kingPos team boardDec

type family SendLeftRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendLeftRay' (At A _) _ _ = False
    SendLeftRay' (At col row) team boardDec = IsLeftUnderAttack (At (L col) row) team (S Z) boardDec

-- The team is for the team BEING ATTACKED
type family IsLeftUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsLeftUnderAttack _ _ Z _ = False 
    IsLeftUnderAttack (At A row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At A row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook)))
    IsLeftUnderAttack (At col row) team (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam team)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsLeftUnderAttack (At (L col) row) team Nat2 boardDec)))
    IsLeftUnderAttack (At col row) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsLeftUnderAttack (At (L col) row) team (S depth) boardDec))

data SendRightRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendRightRay kingPos team boardDec) = SendRightRay' kingPos team boardDec

type family SendRightRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendRightRay' (At H _) _ _ = False
    SendRightRay' (At col row) team boardDec = IsRightUnderAttack (At (R col) row) team (S Z) boardDec

type family IsRightUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsRightUnderAttack _ _ Z _ = False 
    IsRightUnderAttack (At H row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At H row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook)))
    IsRightUnderAttack (At col row) team (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam team)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsRightUnderAttack (At (R col) row) team Nat2 boardDec)))
    IsRightUnderAttack (At col row) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsRightUnderAttack (At (R col) row) team (S depth) boardDec))

data SendAboveRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendAboveRay kingPos team boardDec) = SendAboveRay' kingPos team boardDec

type family SendAboveRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendAboveRay' (At _ Nat8) _ _ = False
    SendAboveRay' (At _ Z) _ _ = False
    SendAboveRay' (At _ Nat9) _ _ = False
    SendAboveRay' (At col row) team boardDec = IsAboveUnderAttack (At col (S row)) team (S Z) boardDec

type family IsAboveUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsAboveUnderAttack _ _ Z _ = False 
    IsAboveUnderAttack (At col Nat8) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat8) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook)))
    IsAboveUnderAttack (At col row) team (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam team)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsAboveUnderAttack (At col (S row)) team Nat2 boardDec)))
    IsAboveUnderAttack (At col row) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsAboveUnderAttack (At col (S row)) team (S depth) boardDec))

data SendBelowRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendBelowRay kingPos team boardDec) = SendBelowRay' kingPos team boardDec

type family SendBelowRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendBelowRay' (At _ Nat1) _ _ = False
    SendBelowRay' (At _ Z) _ _ = False
    SendBelowRay' (At _ Nat9) _ _ = False
    SendBelowRay' (At col (S row)) team boardDec = IsBelowUnderAttack (At col row) team (S Z) boardDec

type family IsBelowUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsBelowUnderAttack _ _ Z _ = False
    IsBelowUnderAttack (At col Nat1) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat1) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook)))
    IsBelowUnderAttack (At col (S row)) team (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam team)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsBelowUnderAttack (At col row) team Nat2 boardDec)))
    IsBelowUnderAttack (At col (S row)) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsRook))))
                (ID 'True)
                (ID (IsBelowUnderAttack (At col row) team (S depth) boardDec))

data SendNWRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendNWRay kingPos team boardDec) = SendNWRay' kingPos team boardDec

type family SendNWRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendNWRay' (At A _) _ _ = False
    SendNWRay' (At _ Nat8) _ _ = False
    SendNWRay' (At _ Z) _ _ = False
    SendNWRay' (At _ Nat9) _ _ = False
    SendNWRay' (At col row) team boardDec
        = IsNWUnderAttack (At (L col) (S row)) team (S Z) boardDec

type family IsNWUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsNWUnderAttack _ _ Z _ = False 
    IsNWUnderAttack (At col Nat8) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat8) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsNWUnderAttack (At A row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At A row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsNWUnderAttack (At col row) White (S Z) boardDec  -- Check if a Black Pawn is positioned to attack the White King
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam White)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam Black) .&. (IsPawn .|. (IsQueen .|. IsBishop)))))
                (ID 'True)
                (ID (IsNWUnderAttack (At (L col) (S row)) White Nat2 boardDec)))
    IsNWUnderAttack (At col row) Black (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam Black)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam White) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsNWUnderAttack (At (L col) (S row)) Black Nat2 boardDec)))
    IsNWUnderAttack (At col row) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsNWUnderAttack (At (L col) (S row)) team (S depth) boardDec))

data SendNERay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendNERay kingPos team boardDec) = SendNERay' kingPos team boardDec

type family SendNERay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendNERay' (At H _) _ _ = False
    SendNERay' (At _ Nat8) _ _ = False
    SendNERay' (At _ Z) _ _ = False
    SendNERay' (At _ Nat9) _ _ = False
    SendNERay' (At col row) team boardDec
        = IsNEUnderAttack (At (R col) (S row)) team (S Z) boardDec

type family IsNEUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsNEUnderAttack _ _ Z _ = False 
    IsNEUnderAttack (At col Nat8) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat8) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsNEUnderAttack (At H row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At H row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsNEUnderAttack (At col row) White (S Z) boardDec  -- Check if a Black Pawn is positioned to attack the White King
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam White)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam Black) .&. (IsPawn .|. (IsQueen .|. IsBishop)))))
                (ID 'True)
                (ID (IsNEUnderAttack (At (R col) (S row)) White Nat2 boardDec)))
    IsNEUnderAttack (At col row) Black (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) (HasTeam Black)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam White) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsNEUnderAttack (At (R col) (S row)) Black Nat2 boardDec)))
    IsNEUnderAttack (At col row) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsNEUnderAttack (At (R col) (S row)) team (S depth) boardDec))

data SendSWRay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendSWRay kingPos team boardDec) = SendSWRay' kingPos team boardDec

type family SendSWRay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendSWRay' (At A _) _ _ = False
    SendSWRay' (At _ Nat1) _ _ = False
    SendSWRay' (At _ Z) _ _ = False
    SendSWRay' (At _ Nat9) _ _ = False
    SendSWRay' (At col (S row)) team boardDec
        = IsSWUnderAttack (At (L col) row) team (S Z) boardDec

type family IsSWUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsSWUnderAttack _ _ Z _ = False 
    IsSWUnderAttack (At col Nat1) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat1) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsSWUnderAttack (At A row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At A row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsSWUnderAttack (At col (S row)) Black (S Z) boardDec  -- Check if a Black Pawn is positioned to attack the Black King
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam Black)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam White) .&. (IsPawn .|. (IsQueen .|. IsBishop)))))
                (ID 'True)
                (ID (IsSWUnderAttack (At (L col) row) Black Nat2 boardDec)))
    IsSWUnderAttack (At col (S row)) White (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam White)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam Black) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsSWUnderAttack (At (L col) row) White Nat2 boardDec)))
    IsSWUnderAttack (At col (S row)) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsSWUnderAttack (At (L col) row) team (S depth) boardDec))

data SendSERay :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (SendSERay kingPos team boardDec) = SendSERay' kingPos team boardDec

type family SendSERay' (p :: Position) (t :: Team) (b :: BoardDecorator) :: Bool where
    SendSERay' (At H _) _ _ = False
    SendSERay' (At _ Nat1) _ _ = False
    SendSERay' (At _ Z) _ _ = False
    SendSERay' (At _ Nat9) _ _ = False
    SendSERay' (At col (S row)) team boardDec
        = IsSEUnderAttack (At (R col) row) team (S Z) boardDec

type family IsSEUnderAttack (p :: Position) (t :: Team) (d :: Nat) (b :: BoardDecorator) :: Bool where
    IsSEUnderAttack _ _ Z _ = False 
    IsSEUnderAttack (At col Nat1) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At col Nat1) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsSEUnderAttack (At H row) team _ boardDec
        = Eval (IsPieceAtWhichDec boardDec (At H row) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop)))
    IsSEUnderAttack (At col (S row)) Black (S Z) boardDec  -- Check if a Black Pawn is positioned to attack the Black King
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam Black)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam White) .&. (IsPawn .|. (IsQueen .|. IsBishop)))))
                (ID 'True)
                (ID (IsSEUnderAttack (At (R col) row) Black Nat2 boardDec)))
    IsSEUnderAttack (At col (S row)) White (S Z) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) (HasTeam White)))
            (ID 'False)
            (If (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam Black) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsSEUnderAttack (At (R col) row) White Nat2 boardDec)))
    IsSEUnderAttack (At col (S row)) team depth boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col (S row)) ((HasTeam (OppositeTeam' team)) .&. (IsQueen .|. IsBishop))))
                (ID 'True)
                (ID (IsSEUnderAttack (At (R col) row) team (S depth) boardDec))

data IsKnightAttacking :: Position -> Team -> BoardDecorator -> Exp Bool
type instance Eval (IsKnightAttacking kingPos team boardDec)
    = Eval (Any ((Flip (IsPieceAtWhichDec boardDec)) (IsKnight .&. HasTeam (OppositeTeam' team))) (Eval (GetAllKnightPositions kingPos)))

-- Checks if a promotion should have occurred
data ShouldHavePromotedCheck :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (ShouldHavePromotedCheck toPos boardDec)
    = ShouldHavePromotedCheck' toPos boardDec

type family ShouldHavePromotedCheck' (t :: Position) (b :: BoardDecorator) :: BoardDecorator where
    ShouldHavePromotedCheck' (At col Nat8) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col Nat8) (IsPawn .&. HasTeam White)))
            (TE' (TL.Text ("Promotion should have occurred at: " ++ TypeShow (At col Nat8) ++ ". Pawns must be promoted when they reach the opposite end of the board.")))
            (ID boardDec)
    ShouldHavePromotedCheck' (At col Nat1) boardDec
        = If' (Eval (IsPieceAtWhichDec boardDec (At col Nat1) (IsPawn .&. HasTeam Black)))
            (TE' (TL.Text ("Promotion should have occurred at: " ++ TypeShow (At col Nat1) ++ ". Pawns must be promoted when they reach the opposite end of the board.")))
            (ID boardDec)
    ShouldHavePromotedCheck' _ boardDec = boardDec

data MovePieceSwitch :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePieceSwitch piece toPos boardDec) = MovePieceSwitch' piece toPos boardDec

type family MovePieceSwitch' (p :: Piece) (pos :: Position) (b :: BoardDecorator) :: BoardDecorator where
    MovePieceSwitch' (MkPiece team Queen info) toPos boardDec = Eval (MovePieceTo (MkPiece team Queen info) toPos boardDec)
    MovePieceSwitch' (MkPiece team Rook info) toPos boardDec = Eval (MovePieceTo (MkPiece team Rook info) toPos boardDec)
    MovePieceSwitch' (MkPiece team Bishop info) toPos boardDec = Eval (MovePieceTo (MkPiece team Bishop info) toPos boardDec)
    MovePieceSwitch' (MkPiece team Knight info) toPos boardDec = Eval (MovePieceTo (MkPiece team Knight info) toPos boardDec)
    MovePieceSwitch' (MkPiece team King info) toPos boardDec = Eval (MoveKing (MkPiece team King info) toPos boardDec)
    MovePieceSwitch' (MkPiece team Pawn info) toPos boardDec = Eval (MovePawn (MkPiece team Pawn info) toPos boardDec)

-- A variant of SetPieceAtDec, which increments the number of moves a piece has done,
-- and sets that piece as the last piece having moved.
-- NOTE: This is the function that correctly sets the BoardDecorator.
data MovePieceTo :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePieceTo piece toPos (Dec board team pos kings moves))
    = Dec (Eval (SetPieceAt (Eval (IncrementMoves piece)) board toPos)) (Eval (PieceTeam piece)) toPos (UpdateKings kings piece toPos) (S moves)

-- Ensuring you don't move into check is handled by MovePiece
data MoveKing :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MoveKing king toPos boardDec) = Eval ( If (Eval (toPos `In` GetCastlePositions (Eval (PieceTeam king)) boardDec))
    ((CastleMoveRook toPos (RookStartPositions (GetMovingTeam boardDec)) . MovePieceTo king toPos) boardDec)
    (MovePieceTo king toPos boardDec))

data CastleMoveRook :: Position -> (Position, Position) -> BoardDecorator -> Exp BoardDecorator
type instance Eval (CastleMoveRook kingPos '(leftRook, rightRook) boardDec)
    = If' (CloserToLeftRook kingPos)
        (SetPieceAtDecClear (Eval ((FromJust . GetPieceAtDec boardDec) leftRook)) boardDec (OneRight kingPos))
        (SetPieceAtDecClear (Eval ((FromJust . GetPieceAtDec boardDec) rightRook)) boardDec (OneLeft kingPos))

type family CloserToLeftRook (x :: Position) :: Bool where
    CloserToLeftRook (At A _) = True
    CloserToLeftRook (At B _) = True
    CloserToLeftRook (At C _) = True
    CloserToLeftRook (At D _) = True
    CloserToLeftRook (At E _) = False
    CloserToLeftRook (At F _) = False
    CloserToLeftRook (At G _) = False
    CloserToLeftRook (At H _) = False

data MovePawn :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (MovePawn (MkPiece team Pawn info) toPos boardDec) =
    If' (Eval (toPos `In` (Eval (GetEnPassantPosition (GetPosition' info) boardDec))))
            ((EnPassantTakeAt team toPos . MovePieceTo (MkPiece team Pawn info) toPos) boardDec)
            (MovePieceTo (MkPiece team Pawn info) toPos boardDec)

data EnPassantTakeAt :: Team -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (EnPassantTakeAt White pos boardDec) = Eval (ClearPieceAtDec (OneDown pos) boardDec)
type instance Eval (EnPassantTakeAt Black pos boardDec) = Eval (ClearPieceAtDec (OneUp pos) boardDec)
