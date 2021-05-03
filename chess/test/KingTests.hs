module KingTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes
import KingCheckTests

isKingTest1 :: True :~: Eval (Eval (IsKing (MkPiece White King TestInfo)) :&&: (IsKing (MkPiece Black King TestInfo)))
isKingTest1 = Refl

isKingTest2 :: False :~: Eval (Eval (IsKing (MkPiece Black Pawn TestInfo)) :||: (IsKing (MkPiece White Queen TestInfo)))
isKingTest2 = Refl

getUnderAttackPositions1 :: True :~: Eval (Eval (GetAdjacent (At F Nat5)) :=:=: Eval (GetUnderAttackPositions White (Eval (SetPieceAtDec (MkPiece White King TestInfo) EmptyDec (At F Nat5)))))
getUnderAttackPositions1 = Refl

getUnderAttackPositions2 :: False :~: Eval ((At F Nat3) `In` (Eval (GetUnderAttackPositions White (Eval (SetPiecesAtDec '[ '(MkPiece White Rook TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At F Nat4) ] EmptyDec)))))
getUnderAttackPositions2 = Refl

getUnderAttackPositions3 :: ('[] :: [Position]) :~: Eval (GetUnderAttackPositions Black (Eval (SetPieceAtDec (MkPiece White King TestInfo) EmptyDec (At F Nat5))))
getUnderAttackPositions3 = Refl

-- These first two tests should not type check - the program should throw a type error if
-- either side has a missing King
findKingTest1 :: Proxy (a :: Piece)
findKingTest1 = Proxy @(Eval (FindKing White EmptyDec))

findKingTest2 :: Proxy (a :: Piece)
findKingTest2 = Proxy @(Eval (FindKing Black EmptyDec))

findKingTest3 :: MkPiece White King (Info Z (At D Nat4)) :~: Eval (FindKing White (Eval (SetPieceAtDec (MkPiece White King (Info Z (At A Nat1))) EmptyDec (At D Nat4))))
findKingTest3 = Refl

findKingTest4 :: MkPiece Black King (Info Z (At D Nat4)) :~: Eval (FindKing Black (Eval (SetPieceAtDec (MkPiece Black King (Info Z (At A Nat1))) EmptyDec (At D Nat4))))
findKingTest4 = Refl

findKingPositionTest1 :: At D Nat4 :~: GetKingPosition White (Eval (SetPieceAtDec (MkPiece White King TestInfo) EmptyDec (At D Nat4)))
findKingPositionTest1 = Refl

findKingPositionTest2 :: At D Nat4 :~: GetKingPosition Black (Eval (SetPieceAtDec (MkPiece Black King TestInfo) EmptyDec (At D Nat4)))
findKingPositionTest2 = Refl

kingTestSuite = describe "King Tests" $ do
    describe "IsKing Tests" $ do
      it "1: King pieces should return True" $
        shouldTypecheck isKingTest1
      it "2: Non-King pieces should return False" $
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
    describe "GetKingPosition Tests" $ do
      it "1: GetKingPosition should return the correct position of the White King" $
        shouldTypecheck findKingPositionTest1
      it "2: GetKingPosition should return the correct position of the Black King" $
        shouldTypecheck findKingPositionTest2
    kingCheckTestSuite  -- Defined in KingCheckTests.hs
    describe "GetUnderAttackPositions Tests" $ do
      it "1: A board with a single King should have all under attack positions be all positions adjacent to the king" $
        shouldTypecheck getUnderAttackPositions1
      it "2: A White rook should not be able to attack a position behind a Black piece" $
        shouldTypecheck getUnderAttackPositions2
      it "3: A board with only White pieces should not have no positions under attack by the Black team" $
        shouldTypecheck getUnderAttackPositions3