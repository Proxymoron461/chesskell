module KnightMoveTests where

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

import TestTypes

type WhiteKnight  = MkPiece White Knight (Info Z (At D Nat5))
type WhiteKnightBoard = EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Nothing :-> Nothing :-> Nothing :-> Just WhiteKnight :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just (MkPiece White King (Info Z (At F Nat7))) :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Just (MkPiece Black King (Info Z (At H Nat8))))

type BlackKnight = MkPiece Black Knight (Info Z (At D Nat5))
type BlackKnightBoard = EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Nothing :-> Nothing :-> Nothing :-> Just BlackKnight :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just (MkPiece White King (Info Z (At F Nat7))) :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Just (MkPiece Black King (Info Z (At H Nat8))))

type WhiteKnightDec = Dec WhiteKnightBoard Black (At H Nat8) '(At F Nat7, At H Nat8) Nat1
type BlackKnightDec = Dec BlackKnightBoard White (At F Nat7) '(At F Nat7, At H Nat8) Nat1

whiteCannotGoLeft :: Proxy (a :: BoardDecorator)
whiteCannotGoLeft = Proxy @(Eval (Move (At D Nat5) (At B Nat5) WhiteKnightDec))

whiteCannotGoRight :: Proxy (a :: BoardDecorator)
whiteCannotGoRight = Proxy @(Eval (Move (At D Nat5) (At E Nat5) WhiteKnightDec))

whiteCannotGoUp :: Proxy (a :: BoardDecorator)
whiteCannotGoUp = Proxy @(Eval (Move (At D Nat5) (At D Nat7) WhiteKnightDec))

whiteCannotGoDown :: Proxy (a :: BoardDecorator)
whiteCannotGoDown = Proxy @(Eval (Move (At D Nat5) (At D Nat4) WhiteKnightDec))

whiteCannotGoSW :: Proxy (a :: BoardDecorator)
whiteCannotGoSW = Proxy @(Eval (Move (At D Nat5) (At C Nat4) WhiteKnightDec))

whiteCannotGoSE :: Proxy (a :: BoardDecorator)
whiteCannotGoSE = Proxy @(Eval (Move (At D Nat5) (At E Nat4) WhiteKnightDec))

whiteCannotGoNW :: Proxy (a :: BoardDecorator)
whiteCannotGoNW = Proxy @(Eval (Move (At D Nat5) (At C Nat6) WhiteKnightDec))

whiteCannotGoNE :: Proxy (a :: BoardDecorator)
whiteCannotGoNE = Proxy @(Eval (Move (At D Nat5) (At E Nat6) WhiteKnightDec))

blackCannotGoLeft :: Proxy (a :: BoardDecorator)
blackCannotGoLeft = Proxy @(Eval (Move (At D Nat5) (At A Nat5) BlackKnightDec))

blackCannotGoRight :: Proxy (a :: BoardDecorator)
blackCannotGoRight = Proxy @(Eval (Move (At D Nat5) (At G Nat5) BlackKnightDec))

blackCannotGoUp :: Proxy (a :: BoardDecorator)
blackCannotGoUp = Proxy @(Eval (Move (At D Nat5) (At D Nat8) BlackKnightDec))

blackCannotGoDown :: Proxy (a :: BoardDecorator)
blackCannotGoDown = Proxy @(Eval (Move (At D Nat5) (At D Nat2) BlackKnightDec))

blackCannotGoSW :: Proxy (a :: BoardDecorator)
blackCannotGoSW = Proxy @(Eval (Move (At D Nat5) (At B Nat3) BlackKnightDec))

blackCannotGoSE :: Proxy (a :: BoardDecorator)
blackCannotGoSE = Proxy @(Eval (Move (At D Nat5) (At F Nat3) BlackKnightDec))

blackCannotGoNW :: Proxy (a :: BoardDecorator)
blackCannotGoNW = Proxy @(Eval (Move (At D Nat5) (At B Nat7) BlackKnightDec))

blackCannotGoNE :: Proxy (a :: BoardDecorator)
blackCannotGoNE = Proxy @(Eval (Move (At D Nat5) (At F Nat7) BlackKnightDec))

knightMoveTestSuite :: Test.Hspec.Spec
knightMoveTestSuite = describe "Knight Movement Tests" $ do
    it "1: A White Knight cannot move left in a straight line" $
        shouldNotTypecheck whiteCannotGoLeft
    it "2: A White Knight cannot move right in a straight line" $
        shouldNotTypecheck whiteCannotGoRight
    it "3: A White Knight cannot move up in a straight line" $
        shouldNotTypecheck whiteCannotGoUp
    it "4: A White Knight cannot move down in a straight line" $
        shouldNotTypecheck whiteCannotGoDown
    it "5: A Black Knight cannot move left in a straight line" $
        shouldNotTypecheck blackCannotGoLeft
    it "6: A Black Knight cannot move right in a straight line" $
        shouldNotTypecheck blackCannotGoRight
    it "7: A Black Knight cannot move up in a straight line" $
        shouldNotTypecheck blackCannotGoUp
    it "8: A Black Knight cannot move down in a straight line" $
        shouldNotTypecheck blackCannotGoDown
    it "9: A White Knight cannot move North-West in a straight line" $
        shouldNotTypecheck whiteCannotGoNW
    it "10: A White Knight cannot move North-East in a straight line" $
        shouldNotTypecheck whiteCannotGoNE
    it "11: A White Knight cannot move South-West in a straight line" $
        shouldNotTypecheck whiteCannotGoSW
    it "12: A White Knight cannot move South-East in a straight line" $
        shouldNotTypecheck whiteCannotGoSE
    it "13: A Black Knight cannot move North-West in a straight line" $
        shouldNotTypecheck blackCannotGoNW
    it "14: A Black Knight cannot move North-East in a straight line" $
        shouldNotTypecheck blackCannotGoNE
    it "15: A Black Knight cannot move South-West in a straight line" $
        shouldNotTypecheck blackCannotGoSW
    it "16: A Black Knight cannot move South-East in a straight line" $
        shouldNotTypecheck blackCannotGoSE