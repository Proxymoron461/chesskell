{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))

import Lib

shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))  -- Using Haskell’s do-notation
    case result of
        Right _ -> return ()  -- Test passes
        Left (TypeError msg) -> assertFailure ("Term didn’t compile.")

pawnTest1 :: '[ At "c" 3, At "d" 2] :~: Eval (PawnReachableBelow TestBoard2 (At "d" 4) 2)
pawnTest1 = Refl

main :: IO ()
main = hspec $ do
  describe "Type Tests" $ do
    it "should not allow an Int to be a String" $
      shouldNotTypecheck (4 :: String)
  describe "Pawn Tests" $ do
    -- it "Pawn should be able to move down two spaces" $
    --   shouldTypecheck (Refl :: '[ At "c" 3, At "d" 2] :~: Eval (PawnReachableBelow TestBoard2 (At "d" 4) 2))
    it "1: Pawn that hasn't moved yet should be able to move down 2 spaces" $
      shouldTypecheck pawnReachableBelowTest