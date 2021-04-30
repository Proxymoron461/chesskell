import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

import Movement
import TestTypes
import KingTests
import MiscTests
import CastleTests
import PawnTakeTests
import PromotionTests
import EnPassantTests
import KnightMoveTests
import NoUnderAttackCastleTests
import CannotMoveThroughTests
import CannotTakeOwnTeamTests
import CannotMoveIntoCheckTests
import CannotStayInCheckTests
import CannotCauseCheckTests

----------------------------------------------------------------------------------------------
-- ACTUAL TESTS

-- TODO: Multiple test suites over multiple files??
main :: IO ()
main = hspec $ do
  enPassantTestSuite -- Defined in SideCases.hs
  miscTestSuite -- Defined in MiscTests.hs
  kingTestSuite  -- Defined in KingTests.hs
  -- movementTestSuite -- Defined in Movement.hs
  castleTestSuite -- Defined in CastleTests.hs
  promotionTestSuite -- Defined in PromotionTests.hs
  describe "FIDE Laws Tests" $ do
    pawnTakeTestSuite -- Defined in PawnTakeTests.hs
    knightMoveTestSuite -- Defined in KnightMoveTests.hs
    noCastleUnderAttackTestSuite -- Defined in NoUnderAttackCastleTests.hs
    cannotMoveThroughTestSuite -- Defined in CannotMoveThroughTests.hs
    cannotTakeOwnTeamTestSuite -- Defined in CannotTakeOwnTeamTests.hs
    cannotMoveIntoCheckTestSuite -- Defined in CannotMoveIntoCheckTests.hs
    cannotStayInCheckTestSuite -- Defined in CannotStayInCheckTests.hs
    cannotCauseCheckTestSuite -- defined in CannotCauseCheckTests.hs

    
