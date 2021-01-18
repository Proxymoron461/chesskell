module NoUnderAttackCastleTests where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.HUnit.Lang (Assertion, assertFailure)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

type WhiteQueen1 = MkPiece White Queen (Info Z (At D Nat2) False)
type WhiteQueen2 = MkPiece White Queen (Info Z (At F Nat2) False)
type BlackQueen1 = MkPiece Black Queen (Info Z (At D Nat7) False)
type BlackQueen2 = MkPiece Black Queen (Info Z (At F Nat7) False)
type WhiteNoCastle = (Just (MkPiece White Rook (Info Z (At A Nat1) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1) False))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1) False)))
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just BlackQueen1 :-> Nothing :-> Just BlackQueen2 :-> Nothing :<> Nothing)
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8) False))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8) False)))

type BlackNoCastle = (Just (MkPiece White Rook (Info Z (At A Nat1) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1) False))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1) False)))
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just WhiteQueen1 :-> Nothing :-> Just WhiteQueen2 :-> Nothing :<> Nothing)
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8) False))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8) False)))

type BlackCastleLeft = (Just (MkPiece White Rook (Info Z (At A Nat1) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1) False))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1) False)))
                 :-> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just WhiteQueen2 :-> Nothing :<> Nothing)
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8) False))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8) False)))

type WhiteCastleRight = (Just (MkPiece White Rook (Info Z (At A Nat1) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1) False))) :-> Nothing :-> Nothing :<> Just (MkPiece White Rook (Info Z (At A Nat1) False)))
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> (Nothing :-> Nothing :-> Nothing :-> Just BlackQueen1 :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                 :<> (Just (MkPiece Black Rook (Info Z (At A Nat8) False)) :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8) False))) :-> Nothing :-> Nothing :<> Just (MkPiece Black Rook (Info Z (At H Nat8) False)))

type WhiteCannotCastleDec = Dec WhiteNoCastle Black (At D Nat7) '(At E Nat1, At E Nat8) Nat1
type BlackCannotCastleDec = Dec BlackNoCastle White (At D Nat2) '(At E Nat1, At E Nat8) Nat1

type BlackCastleLeftDec = Dec BlackCastleLeft White (At F Nat2) '(At E Nat1, At E Nat8) Nat1
type WhiteCastleRightDec = Dec WhiteCastleRight Black (At D Nat7) '(At E Nat1, At E Nat8) Nat1

whiteCannotCastleLeft :: Fst' (CanCastle White WhiteCannotCastleDec) :~: False
whiteCannotCastleLeft = Refl

whiteCannotCastleRight :: Snd' (CanCastle White WhiteCannotCastleDec) :~: False
whiteCannotCastleRight = Refl

blackCannotCastleLeft :: Fst' (CanCastle Black BlackCannotCastleDec) :~: False
blackCannotCastleLeft = Refl

blackCannotCastleRight :: Snd' (CanCastle Black BlackCannotCastleDec) :~: False
blackCannotCastleRight = Refl

whiteCanCastleRight :: '(False, True) :~: CanCastle White WhiteCastleRightDec
whiteCanCastleRight = Refl

blackCanCastleLeft :: '(True, False) :~: CanCastle Black BlackCastleLeftDec
blackCanCastleLeft = Refl

noCastleUnderAttackTestSuite :: Test.Hspec.Spec
noCastleUnderAttackTestSuite = describe "No Castling Under Attack Tests" $ do
    it "1: A White King cannot castle left through positions under attack" $
        shouldTypecheck whiteCannotCastleLeft
    it "2: A White King cannot castle right through positions under attack" $
        shouldTypecheck whiteCannotCastleRight
    it "3: A Black King cannot castle left through positions under attack" $
        shouldTypecheck blackCannotCastleLeft
    it "4: A Black King cannot castle right through positions under attack" $
        shouldTypecheck blackCannotCastleRight
    it "5: A White King can castle through positions not in check, even if positions in the opposite direction are in check" $
        shouldTypecheck whiteCanCastleRight
    it "6: A Black King can castle through positions not in check, even if positions in the opposite direction are in check" $
        shouldTypecheck blackCanCastleLeft
    