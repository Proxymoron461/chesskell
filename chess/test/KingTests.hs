module KingTests where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Nat)
import Data.Proxy(Proxy(..))

import Lib
import Vec
import FirstClassFunctions

import TestTypes

isKingTest1 :: True :~: Eval (Eval (IsKing (MkPiece White King TestInfo)) :&&: (IsKing (MkPiece White King TestInfo)))
isKingTest1 = Refl

isKingTest2 :: False :~: Eval (Eval (IsKing (MkPiece Black Pawn TestInfo)) :||: (IsKing (MkPiece White Queen TestInfo)))
isKingTest2 = Refl

getUnderAttackPositions1 :: True :~: Eval (Eval (GetAdjacent (At "f" 5)) :=:=: Eval (GetUnderAttackPositions White (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At "f" 5)))))
getUnderAttackPositions1 = Refl

getUnderAttackPositions2 :: False :~: Eval ((At "f" 3) `In` (Eval (GetUnderAttackPositions White (Eval (SetPiecesAt '[ '(MkPiece White Rook TestInfo, At "f" 5), '(MkPiece Black Pawn TestInfo, At "f" 4) ] EmptyBoard)))))
getUnderAttackPositions2 = Refl

getUnderAttackPositions3 :: ('[] :: [Position]) :~: Eval (GetUnderAttackPositions Black (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At "f" 5))))
getUnderAttackPositions3 = Refl

kingCheckTest1 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Pawn TestInfo, At "e" 6) ] EmptyBoard)))
kingCheckTest1 = Refl

kingCheckTest2 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Rook TestInfo, At "f" 8), '(MkPiece White Queen TestInfo, At "f" 6) ] EmptyBoard)))
kingCheckTest2 = Refl

kingCheckTest3 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Rook TestInfo, At "f" 8), '(MkPiece Black Queen TestInfo, At "f" 6) ] EmptyBoard)))
kingCheckTest3 = Refl

kingCheckTest4 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Rook TestInfo, At "f" 8), '(MkPiece Black Pawn TestInfo, At "f" 6) ] EmptyBoard)))
kingCheckTest4 = Refl

kingCheckTest5 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Pawn TestInfo, At "f" 6) ] EmptyBoard)))
kingCheckTest5 = Refl

type CheckTest6Board = Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At "f" 5), '(MkPiece Black Pawn TestInfo, At "e" 6) ] EmptyBoard)
kingCheckTest6 :: Eval (IsKingInCheck White CheckTest6Board) :~: Eval (Eval (FindKingPosition White CheckTest6Board) `In` Eval (GetUnderAttackPositions (Eval (OppositeTeam White)) CheckTest6Board))
kingCheckTest6 = Refl

-- These first two tests should not type check - the program should throw a type error if
-- either side has a missing King
findKingTest1 :: Proxy (a :: Piece)
findKingTest1 = Proxy @(Eval (FindKing White EmptyBoard))

findKingTest2 :: Proxy (a :: Piece)
findKingTest2 = Proxy @(Eval (FindKing Black EmptyBoard))

findKingTest3 :: MkPiece White King (Info Z (At "d" 4) False) :~: Eval (FindKing White (Eval (SetPieceAt (MkPiece White King (Info Z (At "a" 1) False)) EmptyBoard (At "d" 4))))
findKingTest3 = Refl

findKingTest4 :: MkPiece Black King (Info Z (At "d" 4) False) :~: Eval (FindKing Black (Eval (SetPieceAt (MkPiece Black King (Info Z (At "a" 1) False)) EmptyBoard (At "d" 4))))
findKingTest4 = Refl

findKingPositionTest1 :: At "d" 4 :~: Eval (FindKingPosition White (Eval (SetPieceAt (MkPiece White King TestInfo) EmptyBoard (At "d" 4))))
findKingPositionTest1 = Refl

findKingPositionTest2 :: At "d" 4 :~: Eval (FindKingPosition Black (Eval (SetPieceAt (MkPiece Black King TestInfo) EmptyBoard (At "d" 4))))
findKingPositionTest2 = Refl