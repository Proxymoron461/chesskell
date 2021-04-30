module CannotMoveIntoCheckTests where

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
type BlackQueen1 = MkPiece Black Queen (Info Z (At D Nat7))
type BlackQueen2 = MkPiece Black Queen (Info Z (At F Nat7))
type WhiteNoLeftRight = (Just (MkPiece White Rook (Info Z (At A Nat1))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1)))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1))))
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just BlackQueen1 :-> Nothing :-> Just BlackQueen2 :-> Nothing :<> Nothing)
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8)))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8))))

type BlackNoLeftRight = (Just (MkPiece White Rook (Info Z (At A Nat1))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1)))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1))))
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just WhiteQueen1 :-> Nothing :-> Just WhiteQueen2 :-> Nothing :<> Nothing)
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8))) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8)))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8))))

type WhiteNoLeftRightDec = Dec WhiteNoLeftRight Black (At D Nat7) '(At E Nat1, At E Nat8) Nat1
type BlackNoLeftRightDec = Dec BlackNoLeftRight White (At D Nat2) '(At E Nat1, At E Nat8) Nat1

whiteKingCannotMoveIntoCheck :: Proxy (a :: BoardDecorator)
whiteKingCannotMoveIntoCheck = Proxy @(Eval (Move (At E Nat1) (At F Nat1) WhiteNoLeftRightDec))

blackKingCannotMoveIntoCheck :: Proxy (a :: BoardDecorator)
blackKingCannotMoveIntoCheck = Proxy @(Eval (Move (At E Nat8) (At D Nat8) BlackNoLeftRightDec))

cannotMoveIntoCheckTestSuite :: Test.Hspec.Spec
cannotMoveIntoCheckTestSuite = describe "Cannot Move Into Check Tests" $ do
    it "1: A White King cannot move into the attack path of another piece" $
        shouldNotTypecheck whiteKingCannotMoveIntoCheck
    it "2: A Black King cannot move into the attack path of another piece" $
        shouldNotTypecheck blackKingCannotMoveIntoCheck
