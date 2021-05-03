module Movement where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- moveTest1 :: Eval (NoOfPiecesDec LastMoveTestDec) :~: Eval (NoOfPiecesDec (Eval (Move (At A Nat2) (At A Nat3) LastMoveTestDec)))
-- moveTest1 = Refl

-- moveTest2 :: True :~: Eval (IsPieceAt (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec)) (At E Nat7))
-- moveTest2 = Refl

-- moveTest3 :: True :~: Eval (IsPieceAt (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec)) (At A Nat7))
-- moveTest3 = Refl

type LastMoveTestDec = Eval (SetPiecesAtDec '[ '(MkPiece White Queen TestInfo, At A Nat2), '(MkPiece Black Queen TestInfo, At E Nat7),
                                              '(MkPiece White King TestInfo, At A Nat1), '(MkPiece Black King TestInfo, At E Nat8) ] EmptyDec)
type LastMoveBoardPostMove = (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec))

-- lastMovedTest1 :: True :~: Eval (IsLastPieceMovedAt (At A Nat7) LastMoveBoardPostMove)
-- lastMovedTest1 = Refl

-- lastMovedTest2 :: False :~: Eval (IsLastPieceMovedAt (At E Nat7) (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec)))
-- lastMovedTest2 = Refl

-- lastMovedTest3 :: False :~: Eval (IsLastPieceMovedAt (At A Nat7) (Eval (Move (At E Nat7) (At E Nat2) (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec)))))
-- lastMovedTest3 = Refl

-- clearPieceTest1 :: False :~: Eval (IsPieceAt (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestDec)) (At A Nat2))
-- clearPieceTest1 = Refl

-- clearPieceTest2 :: False :~: Eval (IsPieceAt (Eval (ClearPieceAtDec (At E Nat7) LastMoveTestDec)) (At E Nat7))
-- clearPieceTest2 = Refl

-- movementTestSuite = describe "Movement Tests" $ do
--     describe "Last Moved Piece Tests" $ do
--       it "1: If a piece moves, it should be recorded as the last piece moved on the board" $
--         shouldTypeCheck lastMovedTest1
--       it "2: A piece that did not move should not be recorded as the last piece moved" $
--         shouldTypeCheck lastMovedTest2
--       it "3: A piece that moved Nat2 moves ago should not be recorded as the last piece moved" $
--        shouldTypeCheck lastMovedTest3
--     describe "Move function Tests" $ do
--       it "1: Moving a piece which does not result in a take, should not change the number of pieces on the board" $
--         shouldTypeCheck moveTest1
--       it "2: Moving a piece should not move another piece on the board" $
--         shouldTypeCheck moveTest2
--       it "3: Moving a piece to a position should put the piece at that position" $
--         shouldTypeCheck moveTest3
--     describe "ClearPieceAt Tests" $ do
--       it "1: If a piece moves from A to B, then position A should be empty" $
--         shouldTypeCheck clearPieceTest1
--       it "2: If a position with a piece on it gets cleared, that position should now be empty" $
--         shouldTypeCheck clearPieceTest2