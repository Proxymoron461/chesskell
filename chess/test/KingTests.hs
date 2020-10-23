module KingTests where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- FIXME: Each compiles individually - it's compiling them all that kills it.
-- WHAT THE FLIP

isKingTest1 :: True :~: Eval (Eval (IsKing (MkPiece White King TestInfo)) :&&: (IsKing (MkPiece White King TestInfo)))
isKingTest1 = Refl

isKingTest2 :: False :~: Eval (Eval (IsKing (MkPiece Black Pawn TestInfo)) :||: (IsKing (MkPiece White Queen TestInfo)))
isKingTest2 = Refl

-- TODO: Slow, but doesn't crash compiler
getUnderAttackPositions1 :: True :~: Eval (Eval (GetAdjacent (At F Nat5)) :=:=: Eval (GetUnderAttackPositions White (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At F Nat5)))))
getUnderAttackPositions1 = Refl

-- TODO: Slow, but doesn't crash compiler
getUnderAttackPositions2 :: False :~: Eval ((At F Nat3) `In` (Eval (GetUnderAttackPositions White (Eval (SetPiecesAt '[ '(MkPiece White Rook TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At F Nat4) ] EmptyBoard)))))
getUnderAttackPositions2 = Refl

-- TODO: Slow, but doesn't crash compiler
getUnderAttackPositions3 :: ('[] :: [Position]) :~: Eval (GetUnderAttackPositions Black (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At F Nat5))))
getUnderAttackPositions3 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest1 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At E Nat6) ] EmptyBoard)))
kingCheckTest1 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest2 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece White Queen TestInfo, At F Nat6) ] EmptyBoard)))
kingCheckTest2 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest3 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece Black Queen TestInfo, At F Nat6) ] EmptyBoard)))
kingCheckTest3 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest4 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece Black Pawn TestInfo, At F Nat6) ] EmptyBoard)))
kingCheckTest4 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest5 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At F Nat6) ] EmptyBoard)))
kingCheckTest5 = Refl

-- TODO: Slow, but doesn't crash compiler
type CheckTest6Board = Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At E Nat6) ] EmptyBoard)
kingCheckTest6 :: Eval (IsKingInCheck White CheckTest6Board) :~: Eval (Eval (FindKingPosition White CheckTest6Board) `In` Eval (GetUnderAttackPositions (Eval (OppositeTeam White)) CheckTest6Board))
kingCheckTest6 = Refl

-- These first two tests should not type check - the program should throw a type error if
-- either side has a missing King
findKingTest1 :: Proxy (a :: Piece)
findKingTest1 = Proxy @(Eval (FindKing White EmptyBoard))

findKingTest2 :: Proxy (a :: Piece)
findKingTest2 = Proxy @(Eval (FindKing Black EmptyBoard))

findKingTest3 :: MkPiece White King (Info Z (At D Nat4) False) :~: Eval (FindKing White (Eval (SetPieceAt (MkPiece White King (Info Z (At A Nat1) False)) EmptyBoard (At D Nat4))))
findKingTest3 = Refl

findKingTest4 :: MkPiece Black King (Info Z (At D Nat4) False) :~: Eval (FindKing Black (Eval (SetPieceAt (MkPiece Black King (Info Z (At A Nat1) False)) EmptyBoard (At D Nat4))))
findKingTest4 = Refl

findKingPositionTest1 :: At D Nat4 :~: Eval (FindKingPosition White (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At D Nat4))))
findKingPositionTest1 = Refl

findKingPositionTest2 :: At D Nat4 :~: Eval (FindKingPosition Black (Eval (SetPieceAt (MkPiece Black King TestInfo) EmptyBoard (At D Nat4))))
findKingPositionTest2 = Refl