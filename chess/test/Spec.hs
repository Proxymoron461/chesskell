{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Nat)

import Lib
import Vec
import FirstClassFunctions

----------------------------------------------------------------------------------------------
-- TEST TYPES
-- TODO: Remove these and replace with EDSL stuff
-- NOTE: These boards are upside-down - the first row is the last one visually
type TestPosition = At "a" 1  -- i.e. bottom left
type TestPiece    = MkPiece Black Pawn (Info Z TestPosition)
type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type TestBoard    = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow

type TestWhitePawn = MkPiece White Pawn (Info Z (At "a" 2))
type TestWhitePawn2 = MkPiece White Pawn (Info Z (At "a" 7))
type TestWhitePawn3 = MkPiece White Pawn (Info Z (At "b" 3))
type TestBlackPawn = MkPiece Black Pawn (Info Z (At "b" 8))
type TestBoard2   = EmptyRow
                    :-> (Just TestWhitePawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Nothing :-> Just TestWhitePawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Just TestWhitePawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Just TestBlackPawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)

type TestList = Eval (RangeBetween 0 10)

type EmptyBoard = EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :<> EmptyRow

----------------------------------------------------------------------------------------------
-- TEST FUNCTIONS

pawnTest1 :: '[ At "d" 3, At "d" 2] :~: Eval (PawnReachableBelow TestBoard2 (At "d" 4) 2)
pawnTest1 = Refl

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

pieceCanMoveToWhitePawnTest :: '[ At "a" 3, At "a" 4 ] :~: Eval (PieceCanMoveTo TestWhitePawn TestBoard2)
pieceCanMoveToWhitePawnTest = Refl

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

setPieceAtTest1 :: At "b" 6 :~: Eval (PiecePosition (Eval (FromJust (Eval (GetPieceAt (Eval (SetPieceAt (MkPiece Black Pawn (Info Z (At "d" 2))) EmptyBoard (At "b" 6))) (At "b" 6))))))
setPieceAtTest1 = Refl

-- :kind! Flip (SetPieceAt piece) :: Position -> Board -> Exp Board
-- data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type TestInfo = Info Z (At "a" 1)
data SetPieceAtSwapped :: (Piece, Position) -> Board -> Exp Board
type instance Eval (SetPieceAtSwapped '(piece, pos) board) = Eval (SetPieceAt piece board pos)
setPieceAtTest2 :: 3 :~: Eval (NoOfPieces (Eval (Foldr SetPieceAtSwapped EmptyBoard (Eval (Zip '[MkPiece Black Pawn TestInfo, MkPiece White Pawn TestInfo, MkPiece White King TestInfo] '[At "b" 1, At "b" 2, At "b" 3])))))
setPieceAtTest2 = Refl

setPieceAtTest3 :: 1 :~: Eval (NoOfPieces (Eval (Foldr SetPieceAtSwapped EmptyBoard (Eval (Zip '[MkPiece Black Pawn TestInfo, MkPiece White Pawn TestInfo, MkPiece White King TestInfo] (Eval (Replicate 3 (At "b" 1))))))))
setPieceAtTest3 = Refl

----------------------------------------------------------------------------------------------
-- ACTUAL TESTS

shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))  -- Using Haskell’s do-notation
    case result of
        Right _ -> return ()  -- Test passes
        Left (TypeError msg) -> assertFailure ("Term didn’t compile.")

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
    it "3: Setting a piece down on a taken position replaces that piece" $ 
      shouldTypecheck setPieceAtTest3
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
  describe "Pawn Tests" $ do
    it "1: A Black Pawn that hasn't moved yet should be able to move down 2 spaces" $
      shouldTypecheck pawnTest1
    it "2: A White Pawn with 0 moves should be able to move up 2 spaces" $
      shouldTypecheck pieceCanMoveToWhitePawnTest
    it "3: A Black Pawn should be able to take in the two diagonal spaces below it, including when one is occupied by a White piece" $
      shouldTypecheck pawnTakePositionsBlackTest
    it "4: A White Pawn should not be able to take off the board, or take a space occupied by another White piece" $
      shouldTypecheck pawnTakePositionsWhiteTest
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
    