{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))
import GHC.TypeLits (Nat)

import Lib
import Vec
import FirstClassFunctions

import Movement
import TestTypes
import KingTests

----------------------------------------------------------------------------------------------
-- TEST FUNCTIONS

pawnTest1 :: '[ At "d" 3, At "d" 2] :~: Eval (PawnReachableBelow TestBoard2 (At "d" 4) 2)
pawnTest1 = Refl

whitePawnToQueenTest :: Just (MkPiece White Queen (Info (S Z) (At "d" 8) True)) :~: Eval ((Eval (Move (At "d" 7) (At "d" 8) (Eval (SetPieceAt (MkPiece White Pawn TestInfo) EmptyBoard (At "d" 7))))) >>= (Flip GetPieceAt) (At "d" 8))
whitePawnToQueenTest = Refl

blackPawnToQueenTest :: Just (MkPiece Black Queen (Info (S Z) (At "d" 1) True)) :~: Eval ((Eval (Move (At "d" 2) (At "d" 1) (Eval (SetPieceAt (MkPiece Black Pawn TestInfo) EmptyBoard (At "d" 2))))) >>= (Flip GetPieceAt) (At "d" 1))
blackPawnToQueenTest = Refl

getReachableLeftTest1 :: '[ At "c" 2, At "b" 2, At "a" 2] :~: Eval (AllReachableLeft Black TestBoard2 (At "d" 2))
getReachableLeftTest1 = Refl

getReachableLeftTest2 :: '[ At "c" 2, At "b" 2] :~: Eval (AllReachableLeft White TestBoard2 (At "d" 2))
getReachableLeftTest2 = Refl

getReachableLeftTest3 :: '[ At "b" 1, At "a" 1] :~: Eval (AllReachableLeft White TestBoard (At "c" 1))
getReachableLeftTest3 = Refl

getReachableLeftTest4 :: '[ At "b" 1 ] :~: Eval (AllReachableLeft Black TestBoard (At "c" 1))
getReachableLeftTest4 = Refl

getReachableLeftTest5 :: ('[] :: [Position]) :~: Eval (AllReachableLeft Black TestBoard (At "a" 1))
getReachableLeftTest5 = Refl

pawnReachableAboveTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableAbove TestBoard2 (At "b" 7) 2)
pawnReachableAboveTest1 = Refl

pawnReachableAboveTest2 :: ('[ At "d" 5, At "d" 6] ) :~: Eval (PawnReachableAbove TestBoard2 (At "d" 4) 2)
pawnReachableAboveTest2 = Refl

pawnReachableBelowTest1 :: ('[] :: [Position]) :~: Eval (PawnReachableBelow TestBoard2 (At "a" 3) 2)
pawnReachableBelowTest1 = Refl

getPieceAtTest1 :: Just TestPiece :~: Eval (GetPieceAt TestBoard (At "a" 1))
getPieceAtTest1 = Refl

-- :k VecAtR Z :: Vec n a -> Exp (Maybe a)
getPieceAtTest2 :: Just TestPiece :~: Eval (Join (Eval (Bind ((Flip (!!)) (Eval (NatToMyNat 0))) (Eval (TestBoard !! (Eval (NatToMyNat 0)))))))
getPieceAtTest2 = Refl

-- :kind! VecAt (Z :<> (S Z)) :: MyNat -> Exp (Maybe MyNat)
getPieceAtTest3 :: Just Z :~: Eval (Join (Eval ((Eval ((CW (!!)) <$> Just (Z :<> (S Z)))) <*> Just Z)))
getPieceAtTest3 = Refl

canMoveToTest1 :: True :~: Eval (CanMoveTo (At "a" 7) (At "a" 6) (Eval (SetPieceAt (MkPiece Black Pawn (Info (S Z) TestPosition False)) EmptyBoard (At "a" 7))))
canMoveToTest1 = Refl

canMoveToTest2 :: True :~: Eval (CanMoveTo (At "a" 7) (At "a" 5) (Eval (SetPieceAt (MkPiece Black Pawn (Info Z TestPosition False)) EmptyBoard (At "a" 7))))
canMoveToTest2 = Refl

canMoveToTest3 :: False :~: Eval (CanMoveTo (At "a" 7) (At "a" 5) (Eval (SetPieceAt (MkPiece Black Pawn (Info (S Z) TestPosition False)) EmptyBoard (At "a" 7))))
canMoveToTest3 = Refl

canMoveToTest4 :: False :~: Eval (CanMoveTo (At "a" 7) (At "a" 5) (Eval (SetPieceAt (MkPiece White Pawn TestInfo) (Eval (SetPieceAt (MkPiece Black Pawn TestInfo) EmptyBoard (At "a" 7))) (At "a" 6))))
canMoveToTest4 = Refl

type CanReachBoard = Eval (SetPiecesAt '[ '(MkPiece White Rook TestInfo, At "d" 5), '(MkPiece Black King TestInfo, At "d" 4)] EmptyBoard )
canMoveToTest5 :: False :~: Eval (CanMoveTo (At "d" 5) (At "d" 4) CanReachBoard)
canMoveToTest5 = Refl

canReachTest1 :: True :~: Eval (Eval (Eval (CanMoveTo (At "d" 5) (At "d" 4) CanReachBoard) :==: False) :&&: (Eval (CanReach (At "d" 5) (At "d" 4) CanReachBoard) :==: True))
canReachTest1 = Refl

pieceMoveListWhitePawnTest :: '[ At "a" 3, At "a" 4 ] :~: Eval (PieceMoveList TestWhitePawn TestBoard2)
pieceMoveListWhitePawnTest = Refl

pawnTakePositionsBlackTest :: '[ At "a" 7, At "c" 7] :~: Eval (PawnTakePositions TestBlackPawn TestBoard2)
pawnTakePositionsBlackTest = Refl

pawnTakePositionsWhiteTest :: ('[] :: [Position]) :~: Eval (PawnTakePositions TestWhitePawn TestBoard2)
pawnTakePositionsWhiteTest = Refl

listEqualityTest1 :: 'True :~: Eval (('[] :: [Nat]) :=:=: ('[] :: [Nat]))
listEqualityTest1 = Refl

listEqualityTest2 :: 'True :~: Eval (TestList :=:=: Eval (Reverse TestList))
listEqualityTest2 = Refl

listEqualityTest3 :: 'False :~: Eval (TestList :=:=: (90 ': TestList))
listEqualityTest3 = Refl

knightPositionsTest1 :: 'True :~: Eval (('[At "e" 6, At "e" 2, At "c" 6, At "c" 2, At "b" 5, At "b" 3, At "f" 5, At "f" 3]) :=:=: Eval (GetAllKnightPositions (At "d" 4)))
knightPositionsTest1 = Refl

knightPositionsTest2 :: 'True :~: Eval (('[At "b" 3, At "c" 2 ]) :=:=: Eval (GetAllKnightPositions (At "a" 1)))
knightPositionsTest2 = Refl

allReachableGivenListTest1 :: ('[] :: [Position]) :~: Eval (AllReachableGivenList White TestBoard2 '[ At "a" 2, At "a" 7, At "b" 3 ])
allReachableGivenListTest1 = Refl

allReachableGivenListTest2 :: '[ At "a" 1, At "a" 2, At "a" 7, At "b" 3 ] :~: Eval (AllReachableGivenList Black TestBoard2 '[ At "a" 1, At "a" 2, At "a" 7, At "b" 3 ])
allReachableGivenListTest2 = Refl

getAdjacentTest1 :: 'True :~: Eval ('[At "a" 2, At "b" 1, At "b" 2] :=:=: Eval (GetAdjacent (At "a" 1)))
getAdjacentTest1 = Refl

getAdjacentTest2 :: 'True :~: Eval ('[At "e" 4, At "e" 5, At "e" 6, At "g" 4, At "g" 5, At "g" 6,At "f" 4, At "f" 6] :=:=: Eval (GetAdjacent (At "f" 5)))
getAdjacentTest2 = Refl

getAdjacentTest3 :: 'False :~: Eval (In (At "d" 4) (Eval (GetAdjacent (At "d" 4))))
getAdjacentTest3 = Refl

oppositeTeamTest1 :: White :~: Eval (OppositeTeam Black)
oppositeTeamTest1 = Refl

oppositeTeamTest2 :: Black :~: Eval (OppositeTeam White)
oppositeTeamTest2 = Refl

setPieceAtTest1 :: At "b" 6 :~: Eval (PiecePosition (Eval (FromJust (Eval (GetPieceAt (Eval (SetPieceAt (MkPiece Black Pawn (Info Z (At "d" 2) False)) EmptyBoard (At "b" 6))) (At "b" 6))))))
setPieceAtTest1 = Refl

-- :kind! Flip (SetPieceAt piece) :: Position -> Board -> Exp Board
-- data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
setPieceAtTest2 :: 3 :~: Eval (NoOfPieces (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList '[At "b" 1, At "b" 2, At "b" 3])))))
setPieceAtTest2 = Refl

setPieceAtTest3 :: 1 :~: Eval (NoOfPieces (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList (Eval (Replicate 3 (At "b" 1))))))))
setPieceAtTest3 = Refl

setPiecesAtTest1 :: 3 :~: Eval (NoOfPieces (Eval (SetPiecesAt (Eval (Zip TestPieceList '[At "b" 1, At "b" 2, At "b" 3])) EmptyBoard)))
setPiecesAtTest1 = Refl

setPiecesAtTest2 :: (Eval (Foldr (Uncurry2 SetPieceAtSwapped) EmptyBoard (Eval (Zip TestPieceList '[At "b" 1, At "b" 2, At "b" 3]))))
                    :~: (Eval (SetPiecesAt (Eval (Zip TestPieceList '[At "b" 1, At "b" 2, At "b" 3])) EmptyBoard))
setPiecesAtTest2 = Refl

-- -- TODO: Make en passant tests
-- enPassantTest1 :: True :~: False
-- enPassantTest1 = Refl

----------------------------------------------------------------------------------------------
-- ACTUAL TESTS

shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))  -- Using Haskell’s do-notation
    case result of
        Right _ -> return ()  -- Test passes
        Left (TypeError msg) -> assertFailure ("Term didn’t compile.")

shouldTypeCheck :: NFData a => a -> Assertion
shouldTypeCheck = shouldTypecheck

main :: IO ()
main = hspec $ do
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
    it "1: OppositeTeam White = Black" $
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
    it "3" $
      shouldTypecheck getPieceAtTest3
  describe "CanMoveTo and CanReach Tests" $ do
    describe "CanMoveTo Tests" $ do
      it "1: A black pawn should be able to move to the space directly below it (if it is empty)." $
        shouldTypecheck canMoveToTest1
      it "2: A black pawn that has not moved should be able to move 2 spaces below itself (if both are empty)." $
        shouldTypecheck canMoveToTest2
      it "3: A black pawn that has already moved should not be able to move 2 spaces below itself." $
        shouldTypecheck canMoveToTest3
      it "4: A black pawn that has not moved should not be able to move 2 spaces below itself if the space below it is empty." $
        shouldTypecheck canMoveToTest4
      it "5: A piece should not be able to move to the King's current position, even if it is reachable." $
        shouldTypecheck canMoveToTest5
    describe "CanReach Tests" $ do
      it "1: A piece should be able to reach the King's current position, if they can, but not move to it" $
        shouldTypecheck canReachTest1
  describe "Pawn Tests" $ do
    it "1: A Black Pawn that hasn't moved yet should be able to move down 2 spaces" $
      shouldTypecheck pawnTest1
    it "2: A White Pawn with 0 moves should be able to move up 2 spaces" $
      shouldTypecheck pieceMoveListWhitePawnTest
    it "3: A Black Pawn should be able to take in the two diagonal spaces below it, including when one is occupied by a White piece" $
      shouldTypecheck pawnTakePositionsBlackTest
    it "4: A White Pawn should not be able to take off the board, or take a space occupied by another White piece." $
      shouldTypecheck pawnTakePositionsWhiteTest
    it "5: A White Pawn that reaches the bottom of the board should transform into a White Queen, having moved an additional time." $
      shouldTypecheck whitePawnToQueenTest
    it "6: A Black Pawn that reaches the bottom of the board should transform into a Black Queen, having moved an additional time." $
      shouldTypecheck blackPawnToQueenTest
  describe "Knight Movement Tests" $ do
    it "1: A Knight should have 8 squares around it, in L-shapes, that it can jump to" $
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
  describe "King Tests" $ do
    describe "IsKing Tests" $ do
      it "1: King pieces should return true" $
        shouldTypecheck isKingTest1
      it "2: Non-King pieces should return false" $
        shouldTypecheck isKingTest2
    describe "FindKing Tests" $ do
      it "1: If there is no White King on the board, FindKing should throw an error" $
        shouldNotTypecheck findKingTest1
      it "2: If there is no Black King on the board, FindKing should throw an error" $
        shouldNotTypecheck findKingTest2
      it "3: If there is a White King on the board, FindKing should return it" $
        shouldTypecheck findKingTest3
      it "4: If there is a Black King on the board, FindKing should return it" $
        shouldTypecheck findKingTest4
    describe "FindKingPosition Tests" $ do
      it "1: FindKingPosition should return the correct position of the White King" $
        shouldTypecheck findKingPositionTest1
      it "2: FindKingPosition should return the correct position of the Black King" $
        shouldTypecheck findKingPositionTest2
    describe "IsKingInCheck Tests" $ do
      it "1" $
        shouldTypecheck kingCheckTest1
      it "2" $
        shouldTypecheck kingCheckTest2
      it "3" $
        shouldTypecheck kingCheckTest3
      it "4" $
        shouldTypecheck kingCheckTest4
      it "5: A Pawn cannot put a King into check by simply being able to move to the King's position." $
        shouldTypecheck kingCheckTest5
      it "6: The result of IsKingInCheck should be identical to the result of manually checking if the King is in an attack position" $
        shouldTypecheck kingCheckTest6
  describe "GetUnderAttackPositions Tests" $ do
    it "1: A board with a single King should have all under attack positions be all positions adjacent to the king" $
      shouldTypecheck getUnderAttackPositions1
    it "2: A White rook should not be able to attack a position behind a Black piece" $
      shouldTypecheck getUnderAttackPositions2
    it "3: A board with only White pieces should not have no positions under attack by the Black team" $
      shouldTypecheck getUnderAttackPositions3
  describe "Movement Tests" $ do
    -- describe "Last Moved Piece Tests" $ do
    --   it "1: If a piece moves, it should be recorded as the last piece moved on the board" $
    --     shouldTypeCheck lastMovedTest1
    --   it "2: A piece that did not move should not be recorded as the last piece moved" $
    --     shouldTypeCheck lastMovedTest2
    --   it "3: A piece that moved 2 moves ago should not be recorded as the last piece moved" $
    --    shouldTypeCheck lastMovedTest3
    -- describe "Move function Tests" $ do
    --   it "1: Moving a piece which does not result in a take, should not change the number of pieces on the board" $
    --     shouldTypeCheck moveTest1
    --   it "2: Moving a piece should not move another piece on the board" $
    --     shouldTypeCheck moveTest2
      -- it "3: Moving a piece to a position should put the piece at that position" $
      --   shouldTypeCheck moveTest3
    describe "ClearPieceAt Tests" $ do
      it "1: If a piece moves from A to B, then position A should be empty" $
        shouldTypeCheck clearPieceTest1
      it "2: If a position with a piece on it gets cleared, that position should now be empty" $
        shouldTypeCheck clearPieceTest2

    
