module Movement where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Nat)

import Lib
import Vec
import FirstClassFunctions

import TestTypes

-- REPL TESTS
-- :kind! Eval (IsLastPieceMovedAt (At "e" 6) <$> (Eval (Eval (Just LastMoveTestBoard >>= Move (At "e" 7) (At "e" 6)) >>= Move (At "a" 2) (At "b" 2))))
-- = Just False

-- FIXME: The below cause issues, but work in the REPL. Why??
-- moveTest1 :: Eval (NoOfPieces LastMoveTestBoard) :~: Eval (FromMaybe 0 NoOfPieces (Eval (Move (At "a" 2) (At "a" 3) LastMoveTestBoard)))
-- moveTest1 = Refl

-- moveTest2 :: Just True :~: Eval ((Flip IsPieceAt) (At "e" 7) <$> (Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard)))
-- moveTest2 = Refl

-- moveTest3 :: Just True :~: Eval ((Flip IsPieceAt) (At "e" 6) <$> (Eval ((Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard)) >>= Move (At "e" 7) (At "e" 6))))
-- moveTest3 = Refl

-- type LastMoveTestBoard = Eval (SetPiecesAt '[ '(MkPiece White Queen TestInfo, At "a" 2), '(MkPiece Black Queen TestInfo, At "e" 7),
--                                               '(MkPiece White King TestInfo, At "a" 1), '(MkPiece Black King TestInfo, At "e" 8) ] EmptyBoard)
-- -- type LastMoveBoardPostMove = (Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard))
-- lastMovedTest1 :: True :~: Eval (FromMaybe False (IsLastPieceMovedAt (At "a" 7)) ((Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard))))
-- lastMovedTest1 = Refl

-- lastMovedTest2 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At "e" 7)) ((Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard))))
-- lastMovedTest2 = Refl

-- lastMovedTest3 :: False :~: Eval (FromMaybe True (IsLastPieceMovedAt (At "a" 7)) (Eval ((Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard)) >>= Move (At "e" 7) (At "e" 2))))
-- lastMovedTest3 = Refl

-- clearPieceTest1 :: Just False :~: Eval (((Flip IsPieceAt) (At "a" 2)) <$> (Eval (Move (At "a" 2) (At "a" 7) LastMoveTestBoard)))
-- clearPieceTest1 = Refl

-- clearPieceTest2 :: False :~: Eval (IsPieceAt (Eval (ClearPieceAt (At "e" 7) LastMoveTestBoard)) (At "e" 7))
-- clearPieceTest2 = Refl