module MiscTests where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

----------------------------------------------------------------------------------------------
-- TEST FUNCTIONS

pawnTest1 :: '[ At D Nat3, At D Nat2] :~: Eval (PawnReachableBelow TestBoard2 (At D Nat4) 2)
pawnTest1 = Refl

whitePawnToQueenTest :: Just (MkPiece White Queen (Info (S Z) (At D Nat8) True)) :~: Eval ((Eval (Move (At D Nat7) (At D Nat8) (Eval (SetPieceAt (MkPiece White Pawn TestInfo) KingBoard (At D Nat7))))) >>= (Flip GetPieceAt) (At D Nat8))
whitePawnToQueenTest = Refl

blackPawnToQueenTest :: Just (MkPiece Black Queen (Info (S Z) (At D Nat1) True)) :~: Eval ((Eval (Move (At D Nat2) (At D Nat1) (Eval (SetPieceAt (MkPiece Black Pawn TestInfo) KingBoard (At D Nat2))))) >>= (Flip GetPieceAt) (At D Nat1))
blackPawnToQueenTest = Refl

getReachableLeftTest1 :: '[ At C Nat2, At B Nat2, At A Nat2] :~: Eval (AllReachableLeft Black TestBoard2 (At D Nat2))
getReachableLeftTest1 = Refl

getReachableLeftTest2 :: '[ At C Nat2, At B Nat2] :~: Eval (AllReachableLeft White TestBoard2 (At D Nat2))
getReachableLeftTest2 = Refl

getReachableLeftTest3 :: '[ At B Nat1, At A Nat1] :~: Eval (AllReachableLeft White TestBoard (At C Nat1))
getReachableLeftTest3 = Refl

getReachableLeftTest4 :: '[ At B Nat1 ] :~: Eval (AllReachableLeft Black TestBoard (At C Nat1))
getReachableLeftTest4 = Refl

getReachableLeftTest5 :: ('[] :: [Position]) :~: Eval (AllReachableLeft Black TestBoard (At A Nat1))
getReachableLeftTest5 = Refl

pawnReachableAboveTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableAbove TestBoard2 (At B Nat7) 2)
pawnReachableAboveTest1 = Refl

pawnReachableAboveTest2 :: ('[ At D Nat5, At D Nat6] ) :~: Eval (PawnReachableAbove TestBoard2 (At D Nat4) 2)
pawnReachableAboveTest2 = Refl

pawnReachableBelowTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableBelow TestBoard2 (At A Nat3) 2)
pawnReachableBelowTest1 = Refl

getPieceAtTest1 :: Just TestPiece :~: Eval (GetPieceAt TestBoard (At A Nat1))
getPieceAtTest1 = Refl

-- :k VecAtR Z :: Vec n a -> Exp (Maybe a)
getPieceAtTest2 :: Just TestPiece :~: Eval (Join (Eval (Bind ((Flip (!!) (Nat0))) (Eval (TestBoard !! Nat0)))))
getPieceAtTest2 = Refl

-- :kind! VecAt (Z :<> (S Z)) :: Nat -> Exp (Maybe Nat)
getPieceAtTest3 :: Just Z :~: Eval (Join (Eval ((Eval ((CW (!!)) <$> Just (Z :<> (S Z)))) <*> Just Z)))
getPieceAtTest3 = Refl

canMoveToTest1 :: True :~: Eval (CanMoveTo (At A Nat7) (At A Nat6) (Eval (SetPieceAt (MkPiece Black Pawn (Info (S Z) TestPosition False)) EmptyBoard (At A Nat7))))
canMoveToTest1 = Refl

canMoveToTest2 :: True :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAt (MkPiece Black Pawn (Info Z TestPosition False)) EmptyBoard (At A Nat7))))
canMoveToTest2 = Refl

canMoveToTest3 :: False :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAt (MkPiece Black Pawn (Info (S Z) TestPosition False)) EmptyBoard (At A Nat7))))
canMoveToTest3 = Refl

canMoveToTest4 :: False :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAt (MkPiece White Pawn TestInfo) (Eval (SetPieceAt (MkPiece Black Pawn TestInfo) EmptyBoard (At A Nat7))) (At A Nat6))))
canMoveToTest4 = Refl

type CanReachBoard = Eval (SetPiecesAt '[ '(MkPiece White Rook TestInfo, At D Nat5), '(MkPiece Black King TestInfo, At D Nat4)] EmptyBoard )
canMoveToTest5 :: False :~: Eval (CanMoveTo (At D Nat5) (At D Nat4) CanReachBoard)
canMoveToTest5 = Refl

canReachTest1 :: True :~: Eval (Eval (Eval (CanMoveTo (At D Nat5) (At D Nat4) CanReachBoard) :==: False) :&&: (Eval (CanReach (At D Nat5) (At D Nat4) CanReachBoard) :==: True))
canReachTest1 = Refl

pieceMoveListWhitePawnTest :: '[ At A Nat3, At A Nat4 ] :~: Eval (PieceMoveList TestWhitePawn TestBoard2)
pieceMoveListWhitePawnTest = Refl

pawnTakePositionsBlackTest :: '[ At A Nat7, At C Nat7] :~: Eval (PawnTakePositions TestBlackPawn TestBoard2)
pawnTakePositionsBlackTest = Refl

pawnTakePositionsWhiteTest :: ('[] :: [Position]) :~: Eval (PawnTakePositions TestWhitePawn TestBoard2)
pawnTakePositionsWhiteTest = Refl

listEqualityTest1 :: 'True :~: Eval (('[] :: [Nat]) :=:=: ('[] :: [Nat]))
listEqualityTest1 = Refl

listEqualityTest2 :: 'True :~: Eval (TestList :=:=: Eval (Reverse TestList))
listEqualityTest2 = Refl

listEqualityTest3 :: 'False :~: Eval (TestList :=:=: (90 ': TestList))
listEqualityTest3 = Refl

knightPositionsTest1 :: 'True :~: Eval (('[At E Nat6, At E Nat2, At C Nat6, At C Nat2, At B Nat5, At B Nat3, At F Nat5, At F Nat3]) :=:=: Eval (GetAllKnightPositions (At D Nat4)))
knightPositionsTest1 = Refl

knightPositionsTest2 :: 'True :~: Eval (('[At B Nat3, At C Nat2 ]) :=:=: Eval (GetAllKnightPositions (At A Nat1)))
knightPositionsTest2 = Refl

allReachableGivenListTest1 :: ('[] :: [Position]) :~: Eval (AllReachableGivenList White TestBoard2 '[ At A Nat2, At A Nat7, At B Nat3 ])
allReachableGivenListTest1 = Refl

allReachableGivenListTest2 :: '[ At A Nat1, At A Nat2, At A Nat7, At B Nat3 ] :~: Eval (AllReachableGivenList Black TestBoard2 '[ At A Nat1, At A Nat2, At A Nat7, At B Nat3 ])
allReachableGivenListTest2 = Refl

getAdjacentTest1 :: 'True :~: Eval ('[At A Nat2, At B Nat1, At B Nat2] :=:=: Eval (GetAdjacent (At A Nat1)))
getAdjacentTest1 = Refl

getAdjacentTest2 :: 'True :~: Eval ('[At E Nat4, At E Nat5, At E Nat6, At G Nat4, At G Nat5, At G Nat6,At F Nat4, At F Nat6] :=:=: Eval (GetAdjacent (At F Nat5)))
getAdjacentTest2 = Refl

getAdjacentTest3 :: 'False :~: Eval (In (At D Nat4) (Eval (GetAdjacent (At D Nat4))))
getAdjacentTest3 = Refl

oppositeTeamTest1 :: White :~: Eval (OppositeTeam Black)
oppositeTeamTest1 = Refl

oppositeTeamTest2 :: Black :~: Eval (OppositeTeam White)
oppositeTeamTest2 = Refl

setPieceAtTest1 :: At B Nat6 :~: Eval (PiecePosition (Eval (FromJust (Eval (GetPieceAt (Eval (SetPieceAt (MkPiece Black Pawn (Info Z (At D Nat2) False)) EmptyBoard (At B Nat6))) (At B Nat6))))))
setPieceAtTest1 = Refl

-- :kind! Flip (SetPieceAt piece) :: Position -> Board -> Exp Board
-- data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
setPieceAtTest2 :: Nat3 :~: Eval (NoOfPieces (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList '[At B Nat1, At B Nat2, At B Nat3])))))
setPieceAtTest2 = Refl

setPieceAtTest3 :: Nat1 :~: Eval (NoOfPieces (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList (Eval (Replicate 3 (At B Nat1))))))))
setPieceAtTest3 = Refl

setPiecesAtTest1 :: Nat3 :~: Eval (NoOfPieces (Eval (SetPiecesAt (Eval (Zip TestPieceList '[At B Nat1, At B Nat2, At B Nat3])) EmptyBoard)))
setPiecesAtTest1 = Refl

setPiecesAtTest2 :: (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList '[At B Nat1, At B Nat2, At B Nat3]))))
                    :~: (Eval (SetPiecesAt (Eval (Zip TestPieceList '[At B Nat1, At B Nat2, At B Nat3])) EmptyBoard))
setPiecesAtTest2 = Refl