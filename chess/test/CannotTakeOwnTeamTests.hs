module CannotTakeOwnTeamTests where

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

type BlackStartDec = SetLastPosition (At E Nat4) (SetLastTeam StartDec White)
type WhiteStartDec = SetLastPosition (At E Nat4) StartDec

-- whiteBishopCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- whiteBishopCannotTakeOwnTeam = Proxy @(Eval (Move (At C Nat1) (At D Nat2) WhiteStartDec))

-- blackBishopCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- blackBishopCannotTakeOwnTeam = Proxy @(Eval (Move (At F Nat8) (At G Nat7) BlackStartDec))

-- whiteRookCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- whiteRookCannotTakeOwnTeam = Proxy @(Eval (Move (At A Nat1) (At A Nat2) WhiteStartDec))

-- blackRookCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- blackRookCannotTakeOwnTeam = Proxy @(Eval (Move (At H Nat8) (At H Nat7) BlackStartDec))

-- whiteQueenCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- whiteQueenCannotTakeOwnTeam = Proxy @(Eval (Move (At D Nat1) (At D Nat2) WhiteStartDec))

-- blackQueenCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- blackQueenCannotTakeOwnTeam = Proxy @(Eval (Move (At D Nat8) (At C Nat7) BlackStartDec))

-- whiteKingCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- whiteKingCannotTakeOwnTeam = Proxy @(Eval (Move (At D Nat1) (At D Nat2) WhiteStartDec))

-- blackKingCannotTakeOwnTeam :: Proxy (a :: BoardDecorator)
-- blackKingCannotTakeOwnTeam = Proxy @(Eval (Move (At D Nat8) (At E Nat8) BlackStartDec))

-- cannotTakeOwnTeamTestSuite :: Test.Hspec.Spec
-- cannotTakeOwnTeamTestSuite = describe "Cannot Take Tests" $ do
--     it "1: A White Bishop cannot take a piece on the same team" $
--         shouldNotTypecheck whiteBishopCannotTakeOwnTeam
--     it "2: A Black Bishop cannot take a piece on the same team" $
--         shouldNotTypecheck blackBishopCannotTakeOwnTeam
--     it "3: A White Rook cannot take a piece on the same team" $
--         shouldNotTypecheck whiteRookCannotTakeOwnTeam
--     it "4: A Black Rook cannot take a piece on the same team" $
--         shouldNotTypecheck blackRookCannotTakeOwnTeam
--     it "5: A White Queen cannot take a piece on the same team" $
--         shouldNotTypecheck whiteQueenCannotTakeOwnTeam
--     it "6: A Black Queen cannot take a piece on the same team" $
--         shouldNotTypecheck blackQueenCannotTakeOwnTeam
--     it "7: A White King cannot take a piece on the same team" $
--         shouldNotTypecheck whiteKingCannotTakeOwnTeam
--     it "8: A Black King cannot take a piece on the same team" $
--         shouldNotTypecheck blackKingCannotTakeOwnTeam
