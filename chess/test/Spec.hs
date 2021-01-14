{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import Movement
import TestTypes
import KingTests
import SideCases
import MiscTests
import CastleTests

----------------------------------------------------------------------------------------------
-- ACTUAL TESTS

-- TODO: Multiple test suites over multiple files??
main :: IO ()
main = hspec $ do
  -- enPassantTestSuite -- Defined in SideCases.hs
  miscTestSuite -- Defined in MiscTests.hs
  kingTestSuite  -- Defined in KingTests.hs
  -- movementTestSuite -- Defined in Movement.hs
  castleTestSuite -- Defined in CastleTests.hs

    
