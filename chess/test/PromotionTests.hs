module PromotionTests where

import Test.Hspec
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
import FailedPromotionTests

-- FIXME:: Each individual test causes memory issues. Why??
-- whiteRookPromotion :: True :~: Eval (IsPieceAtWhichDec (Eval (PromotePawnMove (At A Nat7) (At A Nat8) Rook BlackLastPromDec)) (At A Nat8) IsRook)
-- whiteRookPromotion = Refl

-- whiteQueenPromotion :: True :~: Eval (IsPieceAtWhichDec (Eval (PromotePawnMove (At A Nat7) (At A Nat8) Queen BlackLastPromDec)) (At A Nat8) IsQueen)
-- whiteQueenPromotion = Refl

-- blackBishopPromotion :: True :~: Eval (IsPieceAtWhichDec (Eval (PromotePawnMove (At C Nat2) (At C Nat1) Pawn WhiteLastPromDec)) (At C Nat1) IsBishop)
-- blackBishopPromotion = Refl

-- blackKnightPromotion :: True :~: Eval (IsPieceAtWhichDec (Eval (PromotePawnMove (At C Nat2) (At C Nat1) Pawn WhiteLastPromDec)) (At C Nat1) IsKnight)
-- blackKnightPromotion = Refl

promotionTestSuite = describe "Promotion Tests" $ do
    failedPromotionTestSuite -- defined in FailedPromotionTests.hs
    -- describe "Correct Promotion Tests" $ do
    --     it "1: Promoting to a Rook should put a Rook at the target position" $
    --         shouldTypeCheck whiteRookPromotion
        -- it "2: Promoting to a Queen should put a Queen at the target position" $
        --     shouldTypeCheck whiteQueenPromotion
        -- it "3: Promoting to a Bishop should put a Bishop at the target position" $
        --     shouldTypeCheck blackBishopPromotion
        -- it "4: Promoting to a Knight should put a Knight at the target position" $
        --     shouldTypeCheck blackKnightPromotion
        