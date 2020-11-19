module Movement where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- FIXME: The below compile individually, but not as a whole.

-- REPL TESTS
-- :kind! Eval (IsLastPieceMovedAt (At E Nat6) <$> (Eval (Eval (Just LastMoveTestBoard >>= Move (At E Nat7) (At E Nat6)) >>= Move (At A Nat2) (At B Nat2))))
-- = Just False

-- -- TODO: VERY slow, but doesn't crash compiler
-- moveTest1 :: Eval (NoOfPieces LastMoveTestBoard) :~: Eval (FromMaybe Nat0 NoOfPieces (Eval (Move (At A Nat2) (At A Nat3) LastMoveTestBoard)))
-- moveTest1 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- moveTest2 :: Just True :~: Eval ((Flip IsPieceAt) (At E Nat7) <$> (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard)))
-- moveTest2 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- moveTest3 :: Just True :~: Eval ((Flip IsPieceAt) (At E Nat6) <$> (Eval ((Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard)) >>= Move (At E Nat7) (At E Nat6))))
-- moveTest3 = Refl

-- type LastMoveTestBoard = Eval (SetPiecesAt '[ '(MkPiece White Queen TestInfo, At A Nat2), '(MkPiece Black Queen TestInfo, At E Nat7),
--                                               '(MkPiece White King TestInfo, At A Nat1), '(MkPiece Black King TestInfo, At E Nat8) ] EmptyBoard)
-- type LastMoveBoardPostMove = (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard))

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest1 :: True :~: Eval (FromMaybe False (IsLastPieceMovedAt (At A Nat7)) ((Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard))))
-- lastMovedTest1 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest2 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At E Nat7)) ((Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard))))
-- lastMovedTest2 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest3 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At A Nat7)) (Eval ((Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard)) >>= Move (At E Nat7) (At E Nat2))))
-- lastMovedTest3 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- clearPieceTest1 :: Just False :~: Eval (((Flip IsPieceAt) (At A Nat2)) <$> (Eval (Move (At A Nat2) (At A Nat7) LastMoveTestBoard)))
-- clearPieceTest1 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- clearPieceTest2 :: False :~: Eval (IsPieceAt (Eval (ClearPieceAt (At E Nat7) LastMoveTestBoard)) (At E Nat7))
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