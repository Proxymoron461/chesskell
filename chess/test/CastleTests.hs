module CastleHelperTests where

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
import FlatBuilders
import MakeSingletons

import TestTypes
import CastleHelperTests

castleTestSuite = describe "Castle Tests" $ do
    castleHelperTestSuite
   --  describe "CanCastle Tests" $ do
   --      it blah blah blah
   --  describe "Castling tests" $ do
   --      it blah blah blah
