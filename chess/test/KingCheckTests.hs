module KingCheckTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- TODO: Slow, but doesn't crash compiler
kingCheckTest1 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At E Nat6) ] EmptyDec)))
kingCheckTest1 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest2 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece White Queen TestInfo, At F Nat6) ] EmptyDec)))
kingCheckTest2 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest3 :: True :~: Eval (IsKingInCheck White (Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece Black Queen TestInfo, At F Nat6) ] EmptyDec)))
kingCheckTest3 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest4 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Rook TestInfo, At F Nat8), '(MkPiece Black Pawn TestInfo, At F Nat6) ] EmptyDec)))
kingCheckTest4 = Refl

-- TODO: Slow, but doesn't crash compiler
kingCheckTest5 :: False :~: Eval (IsKingInCheck White (Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At F Nat6) ] EmptyDec)))
kingCheckTest5 = Refl

-- -- TODO: Slow, but doesn't crash compiler
-- TODO: Set white king position in kingCheckTest6??
type CheckTest6Board = Eval (SetPiecesAtDec '[ '(MkPiece White King TestInfo, At F Nat5), '(MkPiece Black Pawn TestInfo, At E Nat6) ] EmptyDec)
kingCheckTest6 :: Eval (IsKingInCheck White CheckTest6Board) :~: Eval ((GetKingPosition White CheckTest6Board) `In` Eval (GetUnderAttackPositions (Eval (OppositeTeam White)) CheckTest6Board))
kingCheckTest6 = Refl

kingCheckTestSuite = describe "IsKingInCheck Tests" $ do
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