{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import Movement
import TestTypes
import KingTests
import SideCases
import MiscTests
import CastleTests

----------------------------------------------------------------------------------------------
-- ACTUAL TESTS

-- TODO: Multiple test suites over multiple files??
main :: IO ()
main = hspec $ do
  -- describe "List Equality Tests" $ do
  --   it "1: Two empty lists should be equal" $ do
  --     shouldTypecheck listEqualityTest1
  --   it "2: Two non-empty lists with the same elements should be equal" $
  --     shouldTypecheck listEqualityTest2
  --   it "3: Two non-empty lists with different elements should not be equal" $ 
  --     shouldTypecheck listEqualityTest3
  -- describe "SetPieceAt Tests" $ do
  --   it "1: A piece set to a position should then have that position recorded in the piece's info record" $ 
  --     shouldTypecheck setPieceAtTest1
  --   it "2: Setting n pieces down should mean that there are n pieces on the board" $ 
  --     shouldTypecheck setPieceAtTest2
  -- describe "SetPiecesAt (plural) Tests" $ do
  --   it "1: Setting n pieces down should mean that there are n pieces on the board" $ 
  --     shouldTypecheck setPiecesAtTest1
  --   it "2: The result of SetPiecesAt should be identical to repeated applications of SetPieceAt" $ 
  --     shouldTypecheck setPiecesAtTest2
  -- describe "OppositeTeam Tests" $ do
  --   it "1: OppositeTeam White = Black" $
  --     shouldTypecheck oppositeTeamTest1
  --   it "2: OppositeTeam White = Black" $
  --     shouldTypecheck oppositeTeamTest2
  -- describe "GetReachableLeft Tests" $ do
  --   it "1" $
  --     shouldTypecheck getReachableLeftTest1
  --   it "2" $
  --     shouldTypecheck getReachableLeftTest2
  --   it "3" $
  --     shouldTypecheck getReachableLeftTest3
  --   it "4" $
  --     shouldTypecheck getReachableLeftTest4
  --   it "5" $
  --     shouldTypecheck getReachableLeftTest5
  -- describe "PawnReachableAbove Tests" $ do
  --   it "1" $
  --     shouldTypecheck pawnReachableAboveTest1
  --   it "2" $
  --     shouldTypecheck pawnReachableAboveTest2
  -- describe "PawnReachableBelow Tests" $ do
  --   it "1" $
  --     shouldTypecheck pawnReachableBelowTest1
  -- describe "GetPieceAt Tests" $ do
  --   it "1" $
  --     shouldTypecheck getPieceAtTest1
  --   it "2" $
  --     shouldTypecheck getPieceAtTest2
  --   it "3" $
  --     shouldTypecheck getPieceAtTest3
  -- describe "CanMoveTo and CanReach Tests" $ do
  --   describe "CanMoveTo Tests" $ do
  --     it "1: A black pawn should be able to move to the space directly below it (if it is empty)." $
  --       shouldTypecheck canMoveToTest1
  --     it "2: A black pawn that has not moved should be able to move Nat2 spaces below itself (if both are empty)." $
  --       shouldTypecheck canMoveToTest2
  --     it "3: A black pawn that has already moved should not be able to move Nat2 spaces below itself." $
  --       shouldTypecheck canMoveToTest3
  --     it "4: A black pawn that has not moved should not be able to move Nat2 spaces below itself if the space below it is empty." $
  --       shouldTypecheck canMoveToTest4
  --     it "5: A piece should not be able to move to the King's current position, even if it is reachable." $
  --       shouldTypecheck canMoveToTest5
  --   describe "CanReach Tests" $ do
  --     it "1: A piece should be able to reach the King's current position, if they can, but not move to it" $
  --       shouldTypecheck canReachTest1
    -- enPassantTestSuite -- Defined in SideCases.hs
  -- describe "Knight Movement Tests" $ do
  --   it "1: A Knight should have Nat8 squares around it, in L-shapes, that it can jump to" $
  --     shouldTypecheck knightPositionsTest1
  --   it "2: A Knight should not be able to leap off the board" $
  --     shouldTypecheck knightPositionsTest2
  -- describe "GetAdjacent Tests" $ do
  --   it "1: GetAdjacent places should not go off the edge of the board" $
  --     shouldTypecheck getAdjacentTest1
  --   it "2: GetAdjacent places should form a tight ring around the given position" $
  --     shouldTypecheck getAdjacentTest2
  --   it "3: GetAdjacent spots should not contain the given position" $
  --     shouldTypecheck getAdjacentTest3
  -- describe "AllReachableGivenList Tests" $ do
  --   it "1: Spaces taken up by pieces of the same team should not be reachable" $
  --     shouldTypecheck allReachableGivenListTest1
  --   it "2: Spaces taken up by pieces of the opposite team, and empty spaces, should be reachable" $
  --     shouldTypecheck allReachableGivenListTest2
  kingTestSuite  -- Defined in KingTests.hs
  -- describe "GetUnderAttackPositions Tests" $ do
  --   it "1: A board with a single King should have all under attack positions be all positions adjacent to the king" $
  --     shouldTypecheck getUnderAttackPositions1
  --   it "2: A White rook should not be able to attack a position behind a Black piece" $
  --     shouldTypecheck getUnderAttackPositions2
  --   it "3: A board with only White pieces should not have no positions under attack by the Black team" $
  --     shouldTypecheck getUnderAttackPositions3
  -- movementTestSuite -- Defined in Movement.hs
  castleTestSuite -- Defined in CastleTests.hs

    
