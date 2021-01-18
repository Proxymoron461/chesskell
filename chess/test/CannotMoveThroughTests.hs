module CannotMoveThroughTests where

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

type BlackStartDec = SetLastPosition (At E Nat4) (SetLastTeam StartDec White)
type WhiteStartDec = SetLastPosition (At E Nat4) StartDec

whiteBishopCannotMoveThrough :: Proxy (a :: BoardDecorator)
whiteBishopCannotMoveThrough = Proxy @(Eval (Move (At C Nat1) (At E Nat3) WhiteStartDec))

blackBishopCannotMoveThrough :: Proxy (a :: BoardDecorator)
blackBishopCannotMoveThrough = Proxy @(Eval (Move (At F Nat8) (At H Nat6) BlackStartDec))

whiteRookCannotMoveThrough :: Proxy (a :: BoardDecorator)
whiteRookCannotMoveThrough = Proxy @(Eval (Move (At A Nat1) (At A Nat5) WhiteStartDec))

blackRookCannotMoveThrough :: Proxy (a :: BoardDecorator)
blackRookCannotMoveThrough = Proxy @(Eval (Move (At H Nat8) (At H Nat4) BlackStartDec))

whiteQueenCannotMoveThrough :: Proxy (a :: BoardDecorator)
whiteQueenCannotMoveThrough = Proxy @(Eval (Move (At D Nat1) (At D Nat4) WhiteStartDec))

blackQueenCannotMoveThrough :: Proxy (a :: BoardDecorator)
blackQueenCannotMoveThrough = Proxy @(Eval (Move (At D Nat8) (At F Nat6) BlackStartDec))

cannotMoveThroughTestSuite :: Test.Hspec.Spec
cannotMoveThroughTestSuite = describe "Cannot Move Through Tests" $ do
    it "1: A White Bishop cannot move through an occupied square" $
        shouldNotTypecheck whiteBishopCannotMoveThrough
    it "2: A Black Bishop cannot move through an occupied square" $
        shouldNotTypecheck blackBishopCannotMoveThrough
    it "3: A White Rook cannot move through an occupied square" $
        shouldNotTypecheck whiteRookCannotMoveThrough
    it "4: A Black Rook cannot move through an occupied square" $
        shouldNotTypecheck blackRookCannotMoveThrough
    it "5: A White Queen cannot move through an occupied square" $
        shouldNotTypecheck whiteQueenCannotMoveThrough
    it "6: A Black Queen cannot move through an occupied square" $
        shouldNotTypecheck blackQueenCannotMoveThrough
