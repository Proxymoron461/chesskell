module CannotStayInCheckTests where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.HUnit.Lang (Assertion, assertFailure)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

type WhiteQueen1 = MkPiece White Queen (Info Z (At D Nat2))
type WhiteQueen2 = MkPiece White Queen (Info Z (At F Nat2))
type WhiteQueen3 = MkPiece White Queen (Info Z (At E Nat2))
type BlackQueen1 = MkPiece Black Queen (Info Z (At D Nat7))
type BlackQueen2 = MkPiece Black Queen (Info Z (At F Nat7))
type BlackQueen3 = MkPiece Black Queen (Info Z (At E Nat7))
type WhiteInCheck = (Just (MkPiece White Rook (Info Z (At A Nat1))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1)))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1))))
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just BlackQueen1 :-> Just BlackQueen3 :-> Just BlackQueen2 :-> Nothing :<> Nothing)
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8)))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8))))

type BlackInCheck = (Just (MkPiece White Rook (Info Z (At A Nat1))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1)))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1))))
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just WhiteQueen1 :-> Just WhiteQueen3 :-> Just WhiteQueen2 :-> Nothing :<> Nothing)
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8)))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8))))

type WhiteInCheckDec = Dec WhiteInCheck Black (At D Nat7) '(At E Nat1, At E Nat8) Nat1
type BlackInCheckDec = Dec BlackInCheck White (At D Nat2) '(At E Nat1, At E Nat8) Nat1

whiteKingCannotStayInCheck :: Proxy (a :: BoardDecorator)
whiteKingCannotStayInCheck = Proxy @(Eval (Move (At E Nat1) (At E Nat2) WhiteInCheckDec))

blackKingCannotStayInCheck :: Proxy (a :: BoardDecorator)
blackKingCannotStayInCheck = Proxy @(Eval (Move (At E Nat8) (At E Nat7) BlackInCheckDec))

cannotStayInCheckTestSuite :: Test.Hspec.Spec
cannotStayInCheckTestSuite = describe "Cannot Stay In Check Tests" $ do
    it "1: A White King must move out of check if they are in check" $
        shouldNotTypecheck whiteKingCannotStayInCheck
    it "2: A Black King must move out of check if they are in check" $
        shouldNotTypecheck blackKingCannotStayInCheck
