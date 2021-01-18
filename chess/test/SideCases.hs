module SideCases where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))
import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes
import FlatBuilders

import TestTypes

-- type family IsSpaceVulnerableToEnPassant (b :: BoardDecorator) (p :: Position) :: Bool where
--     IsSpaceVulnerableToEnPassant boardDec (At A row) = Eval (At A row `In` Eval (GetEnPassantPosition (At B row) boardDec))
--     IsSpaceVulnerableToEnPassant boardDec (At H row) = Eval (At H row `In` Eval (GetEnPassantPosition (At G row) boardDec))
--     IsSpaceVulnerableToEnPassant boardDec pos = Eval (
--         Eval (pos `In` (Eval (GetEnPassantPosition (OneLeft pos) boardDec)))
--         :&&: (pos `In` (Eval (GetEnPassantPosition (OneRight pos) boardDec))))

-- type WhitePawnFirst = (Eval (Move (At A Nat2) (At A Nat4) StartDec))
-- enPassantTest1 :: True :~: IsSpaceVulnerableToEnPassant WhitePawnFirst (At A Nat4)
-- enPassantTest1 = Refl

-- enPassantTest2 :: False :~: IsSpaceVulnerableToEnPassant ((Eval (Move (At A Nat2) (At A Nat3) StartDec))) (At A Nat3)
-- enPassantTest2 = Refl

-- type BlackPawnFirst = (Eval (Move (At A Nat7) (At A Nat5) StartDec))
-- enPassantTest3 :: True :~: IsSpaceVulnerableToEnPassant BlackPawnFirst (At A Nat5)
-- enPassantTest3 = Refl

-- enPassantTest4 :: False :~: IsSpaceVulnerableToEnPassant ((Eval (Move (At A Nat7) (At A Nat6) StartDec))) (At A Nat6)
-- enPassantTest4 = Refl

-- enPassantTest5 :: False :~: IsSpaceVulnerableToEnPassant ((Eval (Move (At A Nat7) (At A Nat6) WhitePawnFirst))) (At A Nat4)
-- enPassantTest5 = Refl

-- enPassantTest6 :: False :~: IsSpaceVulnerableToEnPassant ((Eval (Move (At A Nat2) (At A Nat3) BlackPawnFirst))) (At A Nat5)
-- enPassantTest6 = Refl

-- enPassantTest1 = chess
--                    pawn _d2 to _d4
--                    pawn _b7 to _b5
--                    pawn _e2 to _e3
--                    pawn _b5 to _b4
--                    pawn _a2 to _a4
--                    pawn _b4 to _a3
--                  end

-- enPassantTest2 = chess
--                    pawn _d2 to _d4
--                    pawn _b7 to _b5
--                    pawn _e2 to _e3
--                    pawn _b5 to _b4
--                    pawn _e3 to _e4
--                    pawn _b4 to _b3
--                    pawn _a2 to _a3
--                    pawn _b3 to _a2
--                  end

-- enPassantTestSuite = describe "En Passant Tests" $ do
--       it "1: If a White Pawn moves forward 2 spaces as its' first move, then it should be vulnerable to en passant" $
--         shouldTypeCheck enPassantTest1
--       it "2: If a White Pawn moves forward 1 space as its' first move, then it should not be vulnerable to en passant" $
--         shouldNotTypecheck enPassantTest2
--     --   it "3: If a Black Pawn moves forward 2 spaces as its' first move, then it should be vulnerable to en passant" $
--     --     shouldTypeCheck enPassantTest3
--     --   it "4: If a Black Pawn moves forward 1 space as its' first move, then it should not be vulnerable to en passant" $
--     --     shouldTypeCheck enPassantTest4
--     --   it "5: If a White Pawn moves forward 2 spaces, but is not the last piece to move, then it should not be vulnerable to en passant" $
--     --     shouldTypeCheck enPassantTest5
--     --   it "6: If a Black Pawn moves forward 2 spaces, but is not the last piece to move, then it should not be vulnerable to en passant" $
--     --     shouldTypeCheck enPassantTest6