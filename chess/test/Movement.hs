module Movement where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Nat)

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- FIXME: The below compile individually, but not as a whole.

-- REPL TESTS
-- :kind! Eval (IsLastPieceMovedAt (At E 6) <$> (Eval (Eval (Just LastMoveTestBoard >>= Move (At E 7) (At E 6)) >>= Move (At A 2) (At B 2))))
-- = Just False

-- TODO: VERY slow, but doesn't crash compiler
moveTest1 :: Eval (NoOfPieces LastMoveTestBoard) :~: Eval (FromMaybe 0 NoOfPieces (Eval (Move (At A 2) (At A 3) LastMoveTestBoard)))
moveTest1 = Refl

-- TODO: VERY slow, but doesn't crash compiler
moveTest2 :: Just True :~: Eval ((Flip IsPieceAt) (At E 7) <$> (Eval (Move (At A 2) (At A 7) LastMoveTestBoard)))
moveTest2 = Refl

-- TODO: VERY slow, but doesn't crash compiler
moveTest3 :: Just True :~: Eval ((Flip IsPieceAt) (At E 6) <$> (Eval ((Eval (Move (At A 2) (At A 7) LastMoveTestBoard)) >>= Move (At E 7) (At E 6))))
moveTest3 = Refl

type LastMoveTestBoard = Eval (SetPiecesAt '[ '(MkPiece White Queen TestInfo, At A 2), '(MkPiece Black Queen TestInfo, At E 7),
                                              '(MkPiece White King TestInfo, At A 1), '(MkPiece Black King TestInfo, At E 8) ] EmptyBoard)
type LastMoveBoardPostMove = (Eval (Move (At A 2) (At A 7) LastMoveTestBoard))

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest1 :: True :~: Eval (FromMaybe False (IsLastPieceMovedAt (At A 7)) ((Eval (Move (At A 2) (At A 7) LastMoveTestBoard))))
-- lastMovedTest1 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest2 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At E 7)) ((Eval (Move (At A 2) (At A 7) LastMoveTestBoard))))
-- lastMovedTest2 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- lastMovedTest3 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At A 7)) (Eval ((Eval (Move (At A 2) (At A 7) LastMoveTestBoard)) >>= Move (At E 7) (At E 2))))
-- lastMovedTest3 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- clearPieceTest1 :: Just False :~: Eval (((Flip IsPieceAt) (At A 2)) <$> (Eval (Move (At A 2) (At A 7) LastMoveTestBoard)))
-- clearPieceTest1 = Refl

-- -- TODO: VERY slow, but doesn't crash compiler
-- clearPieceTest2 :: False :~: Eval (IsPieceAt (Eval (ClearPieceAt (At E 7) LastMoveTestBoard)) (At E 7))
-- clearPieceTest2 = Refl