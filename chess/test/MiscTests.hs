module MiscTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

----------------------------------------------------------------------------------------------
-- TEST FUNCTIONS

pawnTest1 :: '[ At D Nat3, At D Nat2] :~: Eval (PawnReachableBelow TestDec2 (At D Nat4) 2)
pawnTest1 = Refl

getReachableLeftTest1 :: '[ At C Nat2, At B Nat2, At A Nat2] :~: Eval (AllReachableLeft Black TestDec2 (At D Nat2))
getReachableLeftTest1 = Refl

getReachableLeftTest2 :: '[ At C Nat2, At B Nat2] :~: Eval (AllReachableLeft White TestDec2 (At D Nat2))
getReachableLeftTest2 = Refl

getReachableLeftTest3 :: '[ At B Nat1, At A Nat1] :~: Eval (AllReachableLeft White TestDec (At C Nat1))
getReachableLeftTest3 = Refl

getReachableLeftTest4 :: '[ At B Nat1 ] :~: Eval (AllReachableLeft Black TestDec (At C Nat1))
getReachableLeftTest4 = Refl

getReachableLeftTest5 :: ('[] :: [Position]) :~: Eval (AllReachableLeft Black TestDec (At A Nat1))
getReachableLeftTest5 = Refl

pawnReachableAboveTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableAbove TestDec2 (At B Nat7) 2)
pawnReachableAboveTest1 = Refl

pawnReachableAboveTest2 :: ('[ At D Nat5, At D Nat6] ) :~: Eval (PawnReachableAbove TestDec2 (At D Nat4) 2)
pawnReachableAboveTest2 = Refl

pawnReachableBelowTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableBelow TestDec2 (At A Nat3) 2)
pawnReachableBelowTest1 = Refl

getPieceAtTest1 :: Just TestPiece :~: Eval (GetPieceAt TestBoard (At A Nat1))
getPieceAtTest1 = Refl

-- :k VecAtR Z :: Vec n a -> Exp (Maybe a)
getPieceAtTest2 :: Just TestPiece :~: Eval (Join (Eval ((Eval (TestBoard !! Nat0)) >>= ((Flip (!!) (Nat0))))))
getPieceAtTest2 = Refl

canMoveToTest1 :: True :~: Eval (CanMoveTo (At A Nat7) (At A Nat6) (Eval (SetPieceAtDec (MkPiece Black Pawn (Info (S Z) TestPosition)) EmptyDec (At A Nat7))))
canMoveToTest1 = Refl

canMoveToTest2 :: True :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAtDec (MkPiece Black Pawn (Info Z TestPosition)) EmptyDec (At A Nat7))))
canMoveToTest2 = Refl

canMoveToTest3 :: False :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAtDec (MkPiece Black Pawn (Info (S Z) TestPosition)) EmptyDec (At A Nat7))))
canMoveToTest3 = Refl

canMoveToTest4 :: False :~: Eval (CanMoveTo (At A Nat7) (At A Nat5) (Eval (SetPieceAtDec (MkPiece White Pawn TestInfo) (Eval (SetPieceAtDec (MkPiece Black Pawn TestInfo) EmptyDec (At A Nat7))) (At A Nat6))))
canMoveToTest4 = Refl

type CanReachBoard = Eval (SetPiecesAt '[ '(MkPiece White Rook TestInfo, At D Nat5), '(MkPiece Black King TestInfo, At D Nat4)] EmptyBoard )
type CanReachDec = Dec CanReachBoard Black (At A Nat1) '(At E Nat1, At D Nat4) Nat1
canMoveToTest5 :: False :~: Eval (CanMoveTo (At D Nat5) (At D Nat4) CanReachDec)
canMoveToTest5 = Refl

canReachTest1 :: True :~: Eval (Eval (Eval (CanMoveTo (At D Nat5) (At D Nat4) CanReachDec) :==: False) :&&: (Eval (CanReach (At D Nat5) (At D Nat4) CanReachDec) :==: True))
canReachTest1 = Refl

pieceMoveListWhitePawnTest :: '[ At A Nat3, At A Nat4 ] :~: Eval (PieceMoveList TestWhitePawn TestDec2)
pieceMoveListWhitePawnTest = Refl

pawnTakePositionsBlackTest :: '[ At A Nat7 ] :~: Eval (PawnTakePositions True TestBlackPawn TestDec2)
pawnTakePositionsBlackTest = Refl

pawnTakePositionsWhiteTest :: ('[] :: [Position]) :~: Eval (PawnTakePositions True TestWhitePawn TestDec2)
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

allReachableGivenListTest1 :: ('[] :: [Position]) :~: Eval (AllReachableGivenList White TestDec2 '[ At A Nat2, At A Nat7, At B Nat3 ])
allReachableGivenListTest1 = Refl

allReachableGivenListTest2 :: '[ At A Nat1, At A Nat2, At A Nat7, At B Nat3 ] :~: Eval (AllReachableGivenList Black TestDec2 '[ At A Nat1, At A Nat2, At A Nat7, At B Nat3 ])
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

setPieceAtTest1 :: At B Nat6 :~: Eval (PiecePosition (Eval (FromJust (Eval (GetPieceAt (Eval (SetPieceAt (MkPiece Black Pawn (Info Z (At D Nat2))) EmptyBoard (At B Nat6))) (At B Nat6))))))
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

pawnTestSuite = describe "Pawn Tests" $ do
    it "1: A Black Pawn that hasn't moved yet should be able to move down Nat2 spaces" $
      shouldTypecheck pawnTest1
    it "2: A White Pawn with Nat0 moves should be able to move up Nat2 spaces" $
      shouldTypecheck pieceMoveListWhitePawnTest
    it "3: A Black Pawn should be able to take in the two diagonal spaces below it, including when one is occupied by a White piece" $
      shouldTypecheck pawnTakePositionsBlackTest
    it "4: A White Pawn should not be able to take off the board, or take a space occupied by another White piece." $
      shouldTypecheck pawnTakePositionsWhiteTest

miscTestSuite = describe "Misc Tests" $ do
    describe "List Equality Tests" $ do
        it "1: Two empty lists should be equal" $ do
            shouldTypecheck listEqualityTest1
        it "2: Two non-empty lists with the same elements should be equal" $
            shouldTypecheck listEqualityTest2
        it "3: Two non-empty lists with different elements should not be equal" $ 
            shouldTypecheck listEqualityTest3
    describe "SetPieceAt Tests" $ do
        it "1: A piece set to a position should then have that position recorded in the piece's info record" $ 
            shouldTypecheck setPieceAtTest1
        it "2: Setting n pieces down should mean that there are n pieces on the board" $ 
            shouldTypecheck setPieceAtTest2
    describe "SetPiecesAt (plural) Tests" $ do
        it "1: Setting n pieces down should mean that there are n pieces on the board" $ 
            shouldTypecheck setPiecesAtTest1
        it "2: The result of SetPiecesAt should be identical to repeated applications of SetPieceAt" $ 
            shouldTypecheck setPiecesAtTest2
    describe "OppositeTeam Tests" $ do
        it "1: OppositeTeam Black = White" $
            shouldTypecheck oppositeTeamTest1
        it "2: OppositeTeam White = Black" $
            shouldTypecheck oppositeTeamTest2
    describe "GetReachableLeft Tests" $ do
        it "1" $
            shouldTypecheck getReachableLeftTest1
        it "2" $
            shouldTypecheck getReachableLeftTest2
        it "3" $
            shouldTypecheck getReachableLeftTest3
        it "4" $
            shouldTypecheck getReachableLeftTest4
        it "5" $
            shouldTypecheck getReachableLeftTest5
    describe "PawnReachableAbove Tests" $ do
        it "1" $
            shouldTypecheck pawnReachableAboveTest1
        it "2" $
            shouldTypecheck pawnReachableAboveTest2
    describe "PawnReachableBelow Tests" $ do
        it "1" $
            shouldTypecheck pawnReachableBelowTest1
    describe "GetPieceAt Tests" $ do
        it "1" $
            shouldTypecheck getPieceAtTest1
        it "2" $
            shouldTypecheck getPieceAtTest2
    describe "CanMoveTo and CanReach Tests" $ do
        describe "CanMoveTo Tests" $ do
            it "1: A black pawn should be able to move to the space directly below it (if it is empty)." $
                shouldTypecheck canMoveToTest1
            it "2: A black pawn that has not moved should be able to move Nat2 spaces below itself (if both are empty)." $
                shouldTypecheck canMoveToTest2
            it "3: A black pawn that has already moved should not be able to move Nat2 spaces below itself." $
                shouldTypecheck canMoveToTest3
            it "4: A black pawn that has not moved should not be able to move Nat2 spaces below itself if the space below it is empty." $
                shouldTypecheck canMoveToTest4
            it "5: A piece should not be able to move to the King's current position, even if it is reachable." $
                shouldTypecheck canMoveToTest5
        describe "CanReach Tests" $ do
            it "1: A piece should be able to reach the King's current position, if they can, but not move to it" $
                shouldTypecheck canReachTest1
    describe "Knight Movement Tests" $ do
        it "1: A Knight should have Nat8 squares around it, in L-shapes, that it can jump to" $
            shouldTypecheck knightPositionsTest1
        it "2: A Knight should not be able to leap off the board" $
            shouldTypecheck knightPositionsTest2
    describe "GetAdjacent Tests" $ do
        it "1: GetAdjacent places should not go off the edge of the board" $
            shouldTypecheck getAdjacentTest1
        it "2: GetAdjacent places should form a tight ring around the given position" $
            shouldTypecheck getAdjacentTest2
        it "3: GetAdjacent spots should not contain the given position" $
            shouldTypecheck getAdjacentTest3
    describe "AllReachableGivenList Tests" $ do
        it "1: Spaces taken up by pieces of the same team should not be reachable" $
            shouldTypecheck allReachableGivenListTest1
        it "2: Spaces taken up by pieces of the opposite team, and empty spaces, should be reachable" $
            shouldTypecheck allReachableGivenListTest2
    pawnTestSuite  -- Defined earlier in this file